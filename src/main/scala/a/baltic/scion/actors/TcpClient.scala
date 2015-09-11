
package a.baltic.scion.actors

import java.net.InetSocketAddress
import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.Props
import akka.io.Tcp.Connect
import akka.io.Tcp.Close
import akka.io.Tcp.CommandFailed
import akka.io.Tcp.Register
import akka.util.ByteString
import akka.io.Tcp.Write
import akka.io.Tcp.Received
import akka.io.Tcp.ConnectionClosed
import akka.io.Tcp.CloseCommand
import akka.io.Tcp.CloseCommand
import akka.io.{ IO, Tcp }
import akka.io.Tcp.CloseCommand

object TcpClient {
  def props(remote: InetSocketAddress, replies: ActorRef) =
    Props(classOf[TcpClient], remote, replies)
}
class TcpClient(remote: InetSocketAddress, listener: ActorRef) extends Actor {
  import context.system

  IO(Tcp) ! Connect(remote)

  def receive = {
    case CommandFailed(_: Connect) =>
      listener ! "connect failed"
      context stop self
    case c @ akka.io.Tcp.Connected(remote, local) =>
      listener ! c
      val connection = sender()
      connection ! Register(self)
      context become {
        case data: ByteString =>
          connection ! Write(data)
        case CommandFailed(w: Write) =>
          // O/S buffer was full
          listener ! "write failed"
        case Received(data) =>
          listener ! data
        case "close" =>
          connection ! Close
        case _: ConnectionClosed =>
          listener ! "connection closed"
          context stop self
      }
  }
}