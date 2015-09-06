package a.baltic.scion.actors

import a.baltic.scion.util
import java.net.InetSocketAddress
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.Actor
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import akka.event.Logging
import akka.event.LoggingAdapter
import a.baltic.scion.messages.TcpConnectFailedEvent
import a.baltic.scion.messages.TcpConnectSucceededEvent
import a.baltic.scion.messages.TcpWriteFailedEvent
import a.baltic.scion.messages.TcpCloseCommand
import a.baltic.scion.messages.TcpConnectionClosedEvent

object TcpClient {
  def props(remote: InetSocketAddress, replies: ActorRef) =
    Props(classOf[TcpClient], remote, replies)
}

class TcpClient(remote: InetSocketAddress, listener: ActorRef) extends Actor {
  val log = Logging(context.system, this)
  import Tcp._
  import context.system

  IO(Tcp) ! Connect(remote)

  def receive = {
    case CommandFailed(c: Connect) =>
      listener ! TcpConnectFailedEvent(c)
      context stop self

    case c @ Connected(remote, local) => {
      log.info("connected from local {} to remote {}", local, remote)
      listener ! TcpConnectSucceededEvent(remote, local)
      val connection = sender()
      connection ! Register(self)
      context become {
        case data: ByteString => connection ! Write(data)
        case CommandFailed(w: Write) => listener ! TcpWriteFailedEvent
        case Received(data) => listener ! data
        case TcpCloseCommand => connection ! Close
        case _: ConnectionClosed =>
          listener ! TcpConnectionClosedEvent
          context stop self
      }
    }
  }
}
