
package a.baltic.scion.tcp

import a.baltic.scion.util
import java.net.InetSocketAddress
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.Actor
import akka.io.{ IO, Tcp }
import akka.util.ByteString

object Client {
  def props(remote: InetSocketAddress, replies: ActorRef) =
    Props(classOf[Client], remote, replies)
}

class Client(remote: InetSocketAddress, listener: ActorRef) extends Actor {
  import Tcp._
  import context.system

  IO(Tcp) ! Connect(remote)

  def receive = {
        case CommandFailed(_: Connect) =>
          listener ! "connect failed"
          context stop self
     
        case c @ Connected(remote, local) =>
          listener ! c
          val connection = sender()
          connection ! Register(self)
          context become {
            case data: ByteString =>
              println("client asked to send: " + util.hex(data))
              connection ! Write(data)
            case CommandFailed(w: Write) =>
              // O/S buffer was full
              listener ! "write failed"
            case Received(data) =>
              println("client received: " + util.hex(data))
              listener ! data
            case "close" =>
              println("closing connection")
              connection ! Close
            case _: ConnectionClosed =>
              listener ! "connection closed"
              context stop self
          }
      }
    }

