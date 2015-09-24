package a.baltic.scion.actors

import akka.actor.ActorRef
import akka.actor.FSM
import a.baltic.scion.messages.PeerConnected
import a.baltic.scion.domain.payload.HeadersMessage
import a.baltic.scion.messages.HeadersReceived
import a.baltic.scion.messages.BlockReceived

trait HeadersFirstState

case class HeadersFirstData(
    synchPeer: Option[ActorRef],
    targetBlockHeight: Long
)

case object FullySynched extends HeadersFirstState
case object SynchingHeaders extends HeadersFirstState
case object SynchingBlocks extends HeadersFirstState

/**
 * @author andrew
 */
class BlockChainHeadersFirst(peerManager: ActorRef)
  extends FSM[HeadersFirstState, HeadersFirstData] {

  when(FullySynched) {
    case Event(PeerConnected(peer, peerBlockHeight), data) => {
      if (peerBlockHeight > data.targetBlockHeight) {
        goto(SynchingHeaders) using HeadersFirstData(Some(peer), peerBlockHeight)
      } else {
        stay
      }
    }
  }

  when(SynchingHeaders) {
    case Event(HeadersReceived(headers), data) => {
      goto(SynchingBlocks) using data
    }
  }

  when(SynchingBlocks) {
    case Event(BlockReceived(blockHash), data) => {
      val blocksRemaining = Vector()
      val blockHeight = 0L
      if (blocksRemaining.isEmpty) {
        if (blockHeight >= data.targetBlockHeight) {
          goto(FullySynched) using HeadersFirstData(None, blockHeight)
        } else {
          goto(SynchingHeaders) using data
        }
      } else {
        stay using data
      }
    }
  }

  startWith(FullySynched, HeadersFirstData(None, 1))
}
