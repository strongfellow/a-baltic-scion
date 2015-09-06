
package a.baltic.scion.tcp

import a.baltic.scion.util
import java.net.InetSocketAddress
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.Actor
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import akka.event.Logging
import akka.event.LoggingAdapter

object Client {
  def props(remote: InetSocketAddress, replies: ActorRef) =
    Props(classOf[Client], remote, replies)
}

class Client(remote: InetSocketAddress, listener: ActorRef) extends Actor {
  val log = Logging(context.system, this)
  import Tcp._
  import context.system

  IO(Tcp) ! Connect(remote)

  def receive = {
    case CommandFailed(_: Connect) =>
      context stop self

    case c @ Connected(remote, local) => {
      log.info("connected from local {} to remote {}", local, remote)
      listener ! c
      val connection = sender()
      connection ! Register(self)
      context become {
        case data: ByteString =>
          connection ! Write(data)
        case CommandFailed(w: Write) =>
          log.error("command failed: {}", w.failureMessage)
          // O/S buffer was full
          listener ! "write failed"
        case Received(data) =>
          listener ! data
        case "close" =>
          connection ! Close
        case _: ConnectionClosed =>
          log.info("connection closed")
          context stop self
      }
    }
  }
}

