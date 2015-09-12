
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
import akka.actor.FSM
import akka.io.Tcp.ConnectionClosed

case object TcpConnect
sealed trait S
case object A extends S
case object B extends S
case object C extends S

case class TcpData(
    listener: Option[ActorRef],
    tcp: Option[ActorRef],
    listenerMessage: Option[Any],
    tcpMessage: Option[Any]
)


object TcpClient {
  def props(remote: InetSocketAddress, listener: ActorRef) =
    Props(classOf[TcpClient], remote, listener)
}
class TcpClient(remote: InetSocketAddress, listener: ActorRef) extends FSM[S,TcpData] {
  import context.system

  startWith(A, TcpData(Some(listener), None, None, None))

  when(A) {
    case Event(CommandFailed(_: Connect), TcpData(listener, _, _, _)) => {
      goto(C) using TcpData(listener, None, Some("connect failed"), None)
    }
    case Event(c @ akka.io.Tcp.Connected(remote, local), TcpData(listener, _, _, _)) => {
      goto(B) using TcpData(listener, Some(sender()), Some(c), Some(Register(self)))
    }
  }
  when(C) {
    case _ => stay
  }

  when(B) {
    case Event(data: ByteString, TcpData(listener, tcp, _, _)) =>
      goto(B) using TcpData(listener, tcp, None, Some(Write(data)))
    case Event(CommandFailed(w: Write), TcpData(listener, tcp, _, _)) =>
      goto(B) using TcpData(listener, tcp, Some("write failed"), None)
    case Event(Received(data), TcpData(listener, tcp, _, _)) =>
      goto(B) using TcpData(listener, tcp, Some(data), None)
    case Event("close", TcpData(listener, tcp, _, _)) =>
      goto(B) using TcpData(listener, tcp, None, Some(Close))
    case Event(_:ConnectionClosed, TcpData(listener, _, _, _)) =>
      goto(C) using TcpData(listener, None, Some("conection closed"), None)
  }

  onTransition {
    case _ -> _ => {
      log.info("{}", nextStateData)

      for {
        listener <- nextStateData.listener
        data <- nextStateData.listenerMessage
      } listener ! data
      for {
        connection <- nextStateData.tcp
        data <- nextStateData.tcpMessage
      } connection ! data
    }
  }

  onTransition {
    case _ -> C => context stop self
  }

  initialize()
  IO(Tcp) ! Connect(remote)

}