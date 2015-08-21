package a.baltic.scion.domain

import a.baltic.scion.util.checksum

/**
 * @author andrew
 */
package object payload {

  case class BitcoinMessageEnvelope(
    magic: Long,
    payload: BitcoinMessage
  )

  abstract class BitcoinMessage {}

  case class Inventory(
    invType: Long, // 4 bytes
    hash: IndexedSeq[Byte] // 32 bytes
  )

  case class TxIn(
      hash: Vector[Byte],
      index: Long,
      script: Vector[Byte],
      sequence: Long
  )
  
  case class TxOut(
      value: Long,
      script: Vector[Byte]
  )
  
  case class NetworkAddress(
    timestamp: Option[Long], // maybe 4
    services: Long, // 8
    ip: java.net.InetAddress, // 16
    port: Int // 2
  )
  
  case class VersionMessage(
    version: Long, // 4 bytes
    services: Long, // 8 bytes
    timestamp: Long, // 8 bytes
    to: NetworkAddress, // 26
    from: NetworkAddress, // 26
    nonce: Long, // 8
    userAgent: String, // ?
    startHeight: Long, // 4
    relay: Boolean // 1
  ) extends BitcoinMessage

  case class VerackMessage() extends BitcoinMessage
  
  case class AddrMessage(
      addresses: IndexedSeq[NetworkAddress]
  ) extends BitcoinMessage
  
  case class InvMessage(
    inventories: IndexedSeq[Inventory]
  ) extends BitcoinMessage
  
  case class GetDataMessage(
    inventories: IndexedSeq[Inventory]
  ) extends BitcoinMessage
  
  case class NotFoundMessage(
      inventories: IndexedSeq[Inventory]
  ) extends BitcoinMessage
  
  case class GetBlocksMessage(
      version: Long,
      hashes: IndexedSeq[IndexedSeq[Byte]],
      hashStop: IndexedSeq[Byte]
  ) extends BitcoinMessage
  
  case class GetHeadersMessage(
      version: Long,
      hashes: IndexedSeq[IndexedSeq[Byte]],
      hashStop: IndexedSeq[Byte]
  ) extends BitcoinMessage
  
  case class TxMessage(
      version: Long,
      txins: Vector[TxIn],
      txouts: Vector[TxOut],
      lockTime: Long
  ) extends BitcoinMessage


  case class BlockMessage(
      version: Long,
      previousBlock: Vector[Byte],
      merkeRoot: Vector[Byte],
      timestamp: Long,
      bits: Long,
      nonce: Long,
      transactions: Vector[TxMessage]
  ) extends BitcoinMessage
  
  case class HeadersMessage(
      headers: Vector[BlockMessage]
  ) extends BitcoinMessage

  case class GetAddrMessage() extends BitcoinMessage
  case class MemPoolMessage() extends BitcoinMessage

  case class CheckOrderMessage() extends BitcoinMessage
  case class SubmitOrderMessage() extends BitcoinMessage
  case class ReplyMessage() extends BitcoinMessage

  case class PingMessage(nonce: Long) extends BitcoinMessage
  case class PongMessage(nonce: Long) extends BitcoinMessage
  case class RejectMessage(
      message: String,
      ccode: Int,
      reason: String,
      data: Vector[Byte]
  ) extends BitcoinMessage
  case class FilterLoadMessage(
      filter: Vector[Byte],
      numberOfHashFunctions: Long,
      nTweak: Long,
      nFlags: Long
  ) extends BitcoinMessage

  case class FilterAddMessage(
      data: Vector[Byte]
  ) extends BitcoinMessage

  case class FilterClearMessage() extends BitcoinMessage
  case class MerkleBlockMessage(
      version: Long,
      previousBlock: Vector[Byte],
      merkeRoot: Vector[Byte],
      timestamp: Long,
      bits: Long,
      nonce: Long,
      numTx: Long,
      hashes: Vector[Vector[Byte]],
      flags: Vector[Byte]

  ) extends BitcoinMessage
  case class AlertMessage() extends BitcoinMessage

}
