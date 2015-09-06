package a.baltic.scion

import akka.io.Tcp.Connect
import java.net.InetSocketAddress

/**
 * @author andrew
 */
object messages {

  sealed trait Message
  case class PeerMessage() extends Message
  case class NodeMessage() extends Message

  case class ParseFailedEvent(bytes: IndexedSeq[Byte]) extends Message

  /**
   * Tcp Commands
   */
  case object TcpCloseCommand

  /**
   * Tcp Events
   */
  case object TcpWriteFailedEvent extends Message
  case object TcpConnectionClosedEvent extends Message
  case class TcpConnectFailedEvent(c: Connect) extends Message
  case class TcpConnectSucceededEvent(
      reomte: InetSocketAddress, local: InetSocketAddress) extends Message
}
