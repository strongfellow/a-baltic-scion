
package a.baltic.scion.actors

import akka.actor.Actor
import akka.actor.FSM
import a.baltic.scion.domain.payload.VersionMessage
import akka.actor.ActorRef
import a.baltic.scion.domain.payload.HeadersMessage
import a.baltic.scion.Hash
import a.baltic.scion.domain.payload.GetHeadersMessage
import a.baltic.scion.util
import a.baltic.scion.messages.SendGetHeadersMessage
import a.baltic.scion.domain.payload.HeadersMessage
import a.baltic.scion.bitcoin.BlockLocator

abstract trait BlockChainState
case object S0 extends BlockChainState
case object S1 extends BlockChainState
case class BlockChainData(targetHeight: Long,
    synchPeer: Option[ActorRef],
    headers: Vector[Hash])


class BlockChain extends FSM[BlockChainState, BlockChainData] {
  val genesisHash: Vector[Byte] = a.baltic.scion.util.unHex(
          "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f").toVector.reverse


  when(S0) {
    case Event(v: VersionMessage, data: BlockChainData) => {
      if (v.startHeight > data.targetHeight) {
        goto(S1) using BlockChainData(v.startHeight, Some(sender), data.headers)
      } else {
        stay using data
      }
    }
  }

  onTransition {
    case S0 -> S1 => {
      nextStateData.synchPeer.get ! SendGetHeadersMessage(
          BlockLocator.blockLocator(nextStateData.headers))
    }
  }

  when(S1) {
    case Event(message: HeadersMessage, data) =>
      for {
        x <- message.headers
      } log.info("{}: {} -> {}", x.transactions.length, util.hex(util.headerHash(x).reverse),
          util.hex(x.header.previousBlock.reverse))
      log.info("message: {}", message)
      log.info("data: {}", data)
      stay using data
  }

  startWith(S0, BlockChainData(1, None, Vector(genesisHash)))

}
