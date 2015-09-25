package a.baltic.scion.actors

import akka.actor.Actor
import akka.event.Logging
import a.baltic.scion.domain.payload.BitcoinMessage
import a.baltic.scion.domain.payload.VersionMessage
import a.baltic.scion.domain.payload.NetworkAddress
import a.baltic.scion.domain.payload.PingMessage
import akka.actor.FSM
import akka.actor.ActorRef
import a.baltic.scion.messages.TcpConnectSucceededEvent
import java.net.InetSocketAddress
import a.baltic.scion.messages.PeerConnectCommand
import a.baltic.scion.messages.TcpConnectCommand
import akka.actor.Props
import a.baltic.scion.domain.payload.VerackMessage
import a.baltic.scion.messages.SendGetHeadersMessage
import a.baltic.scion.domain.payload.GetHeadersMessage
import a.baltic.scion.domain.payload.HeadersMessage
import a.baltic.scion.domain.payload.InvMessage
import a.baltic.scion.domain.payload.BlockMessage

sealed trait State

case object Unconnected extends State
case object VersionMessageSent extends State
case object VersionMessageReceived extends State
case object Connecting extends State
case object Connected extends State
case object ReadInitialized extends State
case object WriteInitialized extends State
case object ReadWriteInitialized extends State

sealed trait Data
case object Uninitialized extends Data
case class Connected(
    remote:InetSocketAddress, local: InetSocketAddress) extends Data

object PeerConnection {
  def props(blockChain: ActorRef, serializer: ActorRef) =  Props(classOf[PeerConnection], blockChain, serializer)
}

/**
 * @author andrew
 */

class PeerConnection(blockChain: ActorRef, serializer: ActorRef) extends FSM[State, Data] {

  startWith(Unconnected, Uninitialized)

  when(Unconnected) {
    case Event(akka.io.Tcp.Connected(remote, local), data) => {
      log.info("yeah baby we are connected from local {} to remote {}", local, remote)
      val timestamp = System.currentTimeMillis() / 1000L
      val v = VersionMessage(
          70002L,
          1L,
          timestamp,
          NetworkAddress(
              None,
              1,
              a.baltic.scion.util.networkAddressToBytes(remote),
              remote.getPort),
          NetworkAddress(
              None,
              1,
              a.baltic.scion.util.networkAddressToBytes(local),
              local.getPort),
          100L,
          "/a-baltic-scion/",
          0,
          true)
      serializer ! v
      goto(VersionMessageSent) using data
    }
  }
  when(VersionMessageSent) {
    case Event(v: VersionMessage, data) => {
      // serializer ! VerackMessage()
      log.info("version message received by PeerConnection: {}", v)
      blockChain ! v
      goto(VersionMessageReceived) using data
    }
  }

  when(VersionMessageReceived) {
    case Event(SendGetHeadersMessage(hashes), data) => {
      val hashStop = a.baltic.scion.util.unHex("0000000000000000000000000000000000000000000000000000000000000000").toVector
      val getHeadersMessage = GetHeadersMessage(70002, hashes, hashStop)
      serializer ! getHeadersMessage
      serializer ! PingMessage(1L)
      stay using data
    }

    case Event(h: HeadersMessage, data) => {
      blockChain ! h
      stay using data
    }

    case Event(block: BlockMessage, data) => {
      blockChain ! block
      stay using data
    }

    case Event(message, data) =>
      log.info("received {}", message)
      stay using data
  }

  whenUnhandled {
    case x => log.info("unhandled: {}", x)
    stay()
  }

  initialize()
}
