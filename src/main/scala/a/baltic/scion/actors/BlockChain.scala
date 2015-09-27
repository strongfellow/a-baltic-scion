
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
import a.baltic.scion.domain.payload.BlockMessage

abstract trait BlockChainState
case object FullySynched extends BlockChainState
case object BlocksSynchedHeadersLagging extends BlockChainState
case object BlocksLagging extends BlockChainState
case object HeadersSynched extends BlockChainState

case class BlockChainData(
    targetHeight: Long,
    synchPeer: Option[ActorRef],
    headers: Vector[Hash],
    missingBlocks: Set[Hash])

class BlockChain extends FSM[BlockChainState, BlockChainData] {

  val genesisBlock: Vector[Byte] = a.baltic.scion.util.unHex(
      "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a29ab5f49ffff001d1dac2b7c0101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac00000000"
  ).toVector

  val genesisHash: Vector[Byte] = a.baltic.scion.util.unHex(
          "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f").toVector.reverse

  when(FullySynched) {
    case Event(v: VersionMessage, data: BlockChainData) => {
      if (v.startHeight > data.targetHeight) {
        goto(BlocksSynchedHeadersLagging) using BlockChainData(
            v.startHeight,
            Some(sender),
            data.headers,
            data.missingBlocks)
      } else {
        stay using data
      }
    }
  }

  onTransition {
    case _ -> BlocksSynchedHeadersLagging => {
      nextStateData.synchPeer.get ! SendGetHeadersMessage(
          BlockLocator.blockLocator(nextStateData.headers))
    }
  }

  when(BlocksSynchedHeadersLagging) {
    case Event(message: HeadersMessage, data) =>
      val newHeaders = message.headers.foldLeft(data.headers) {
        (hs, x) => {
          val tip = hs.last
          val prev = x.header.previousBlock
          if (prev == tip) {
//            log.info("YIPPEE")
            hs :+ util.headerHash(x)
          } else {
            log.info("WHAMMEE")
            hs
          }
        }
      }
      log.info("blockChain height: {}", newHeaders.length)
      val nextStateData = BlockChainData(
          data.targetHeight,
          data.synchPeer,
          newHeaders,
          newHeaders.slice(data.headers.length, newHeaders.length).toSet
          )
      if (nextStateData.missingBlocks.isEmpty) {
        stay using nextStateData
      } else {
        goto(BlocksLagging) using nextStateData
      }
      /**
      if (newHeaders.length >= data.targetHeight) {
        goto(HeadersSynched) using nextStateData
      } else {
        goto(BlocksSynchedHeadersLagging) using nextStateData
      }
      *
      */
  }

  when(BlocksLagging) {
    case Event(block: BlockMessage,
        BlockChainData(targetHeight, synchPeer, headers, missingBlocks)) =>
      val headerHash = util.headerHash(block)
      val newMissingBlocks = missingBlocks - headerHash
      val nextStateData = BlockChainData(targetHeight, synchPeer, headers, newMissingBlocks)
      val nextState:BlockChainState = if (newMissingBlocks.isEmpty) {
        if (targetHeight == headers.length) {
          FullySynched
        } else {
          BlocksSynchedHeadersLagging
        }
      } else {
        BlocksLagging
      }
      goto(nextState) using nextStateData
  }

  onTransition {
    case _ -> HeadersSynched =>
      log.info("BLOCKCHAIN HEADERS SYNCHED")
  }

  when(HeadersSynched) {
    case _ => stay
  }

  startWith(FullySynched,
      BlockChainData(1,
          None,
          Vector(genesisHash),
          Set.empty))

}
