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

  val genesisHash: Vector[Byte] = Vector(0x00, 0x00, 0x00, 0x00,
               0x00, 0x19, 0xd6, 0x68,
               0x9c, 0x08, 0x5a, 0xe1,
               0x65, 0x83, 0x1e, 0x93,
               0x4f, 0xf7, 0x63, 0xae,
               0x46, 0xa2, 0xa6, 0xc1,
               0x72, 0xb3, 0xf1, 0xb6,
               0x0a, 0x8c, 0xe2, 0x6f).map(x => x.toByte)

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
