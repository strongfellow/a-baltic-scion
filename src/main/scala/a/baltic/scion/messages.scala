package a.baltic.scion

import akka.io.Tcp.Connect
import java.net.InetSocketAddress
import akka.actor.ActorRef

/**
 * @author andrew
 */
object messages {

  sealed trait Command
  case object Connect extends Command
  case class Register(listener: ActorRef) extends Command

  sealed trait Message
  case class PeerMessage() extends Message
  case class NodeMessage() extends Message

  case class InvalidNetworkEvent(expected:Long, actual:Long) extends Message
  case class ParseFailedEvent(bytes: IndexedSeq[Byte]) extends Message

  /**
   * Tcp Commands
   */
  case class TcpConnectCommand(listener: ActorRef)
  case object TcpCloseCommand

  /**
   * Tcp Events
   */
  case object TcpWriteFailedEvent extends Message
  case object TcpConnectionClosedEvent extends Message
  case class TcpConnectFailedEvent(c: Connect) extends Message
  case class TcpConnectSucceededEvent(
      reomte: InetSocketAddress, local: InetSocketAddress) extends Message

  case object PeerConnectCommand

  case class PeerConnected(peer: ActorRef, blockHeight: Long)
  case class HeadersReceived(headers: Vector[Hash])
  case class BlockReceived(blockHash: Hash)

  case class SendGetHeadersMessage(hashes: Vector[Hash])

}
