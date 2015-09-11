package a.baltic.scion.actors

import akka.actor.Actor
import akka.event.Logging
import a.baltic.scion.domain.payload.BitcoinMessage
import a.baltic.scion.domain.payload.VersionMessage
import a.baltic.scion.domain.payload.NetworkAddress
import akka.actor.FSM
import akka.actor.ActorRef
import a.baltic.scion.messages.TcpConnectSucceededEvent
import java.net.InetSocketAddress
import a.baltic.scion.messages.PeerConnectCommand
import a.baltic.scion.messages.TcpConnectCommand

sealed trait State

case object Unconnected extends State
case object Connecting extends State
case object Connected extends State
case object ReadInitialized extends State
case object WriteInitialized extends State
case object ReadWriteInitialized extends State

sealed trait Data
case object Uninitialized extends Data
case class Connected(
    remote:InetSocketAddress, local: InetSocketAddress) extends Data

/**
 * @author andrew
 */

class PeerConnection(serializer: ActorRef) extends FSM[State, Data] {

  startWith(Unconnected, Uninitialized)

  when(Unconnected) {
    case Event(PeerConnectCommand, data) =>
      goto(Connecting) using data
  }

  when(Connecting) {
    case Event(TcpConnectSucceededEvent(remote, local), data) =>
      goto(Connected) using Connected(remote, local)
  }

  when(Connected) {
    case Event(x, data) =>
      log.info("received: {}", x)
      stay using data
  }

  onTransition {
    case Unconnected -> Connecting =>
      serializer ! TcpConnectCommand
    case Connecting -> Connected =>
      nextStateData match {
        case Connected(remote, local) =>
          val msg = VersionMessage(
            70002L, // protocol version
            1L, // services
            System.currentTimeMillis() / 1000L, // timestamp
            NetworkAddress(None, 1, Vector(remote.getAddress().getAddress(): _*), remote.getPort()), // remote address
            NetworkAddress(None, 1, Vector(local.getAddress().getAddress(): _*), local.getPort()), // local address
            7284544412836900411L, // little endian nonce
            "/ABalticScion/", // UserAgent
            0L,
            true)
      }
  }

  when(Connected) {
    case Event(x, data) =>
      log.info("connected; received {}", x)
      goto(Connected) using data
  }

}
