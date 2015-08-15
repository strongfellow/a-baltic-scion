package a.baltic.scion.domain

import a.baltic.scion.util.checksum
import a.baltic.scion.util.{littleEndian4, littleEndian8}
import a.baltic.scion.util.{parseLittleEndian}
import a.baltic.scion.util.varInt
import a.baltic.scion.util.network2

/**
 * @author andrew
 */
package object payload {
/**
  def parseBitcoinNetworkMessage(bytes: Seq[Byte]): Option[BitcoinNetworkMessage] = {
    val magic: Option[Long] = parseLittleEndian(bytes, 4)
    val command = parseCommand(bytes.drop(4).take(12))
  }

  trait BitcoinSerializable {
    def serialize(): Seq[Byte]
  }
  sealed abstract class MessagePayload extends BitcoinSerializable {}

  sealed case class BitcoinNetworkMessage(
    magic: Long, // 4 bytes
    command: String, // up to 12 ascii characters
    payload: MessagePayload
  ) extends BitcoinSerializable {
    override def serialize(): Seq[Byte] = {
      val p = payload.serialize()
      (littleEndian4(magic)
          ++ command.map(_.toByte) ++ Seq.fill(12 - command.length)(0.toByte)
          ++ littleEndian4(p.length)
          ++ littleEndian4(checksum(p))
          ++ p)
    }
  }
  
  case class BlockHeader(
    version: Long, // 4 bytes
    previousBlock: Vector[Byte],
    merkleRoot: Vector[Byte],
    timestamp: Long, // 4 bytes
    bits: Long, // 4 bytes
    nonce: Long, // 4 bytes
    transactionCount: Long // varInt
  ) extends BitcoinSerializable {
    def serialize(): Seq[Byte] =  {
      (littleEndian4(version)
          ++ previousBlock
          ++ merkleRoot
          ++ littleEndian4(timestamp)
          ++ littleEndian4(bits)
          ++ littleEndian4(nonce)
          ++ varInt(transactionCount))
    }
  }
  
  case class NetworkAddress(
    timestamp: Long, // 4
    services: Long, // 8
    ip: Vector[Byte], // 16
    port: Int // 2
  ) extends BitcoinSerializable {
    override def serialize(): Seq[Byte] = {
      (littleEndian4(timestamp)
          ++ littleEndian8(services)
          ++ ip
          ++ network2(port))
    }
  }

  case class InventoryVector(
    vectorType: Long, // 4 bytes
    vectorHash: Vector[Byte] // 32 bytes
  ) extends BitcoinSerializable {
    
  }

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
  ) extends MessagePayload

  case class VerackMessage() extends MessagePayload

  case class AddrMessage(
      addresses: Vector[NetworkAddress] // 30 bytes each
    ) extends MessagePayload

  case class InvMessage(
    invs: Vector[InventoryVector] // 36 bytes each
  ) extends MessagePayload

  case class GetDataMessage(
    invs: Vector[InventoryVector] // 36 bytes each
  ) extends MessagePayload

  case class NotFoundMessage(
    invs: Vector[InventoryVector] // 36 bytes each
  ) extends MessagePayload

  case class GetBlocksMessage(
    version: Long, // 4 bytes; protocol version
    blockLocatorHashes: Vector[Vector[Byte]], // 32 bytes each
    hashStop: Vector[Byte] // 32 bytes
  ) extends MessagePayload

  case class GetHeadersMessage(
    version: Long, // 4 bytes
    blockLocatorHashes: Vector[Vector[Byte]], // 32 bytes each
    hashStop: Vector[Byte]
  ) extends MessagePayload

  case class TxIn(
    hash: Vector[Byte], // 32 bytes each
    index: Long, // 4 bytes
    script: Vector[Byte], // ? bytes
    sequenceNumber: Long // 4 bytes
  )
  
  case class TxOut(
    value: Long, // 8 bytes,
    script: Vector[Byte] // ? bytes
  )
  
  case class TxMessage(
    version: Long, // 4 bytes
    txins: Vector[TxIn],
    txouts: Vector[TxOut],
    lockTime: Long
  ) extends MessagePayload

  case class BlockMessage(
    version: Long, // 4 bytes
    previousBlock: Vector[Byte], // 32 bytes
    merkleRoot: Vector[Byte], // 32 bytes
    timestamp: Long, // 4 bytes
    bits: Long, // 4 bytes difficulty target
    nonce: Long, // 4 bytes
    transactions: Vector[TxMessage]
  ) extends MessagePayload

  /**
   * response to a getHeaders message
   */
  case class HeadersMessage(
    headers: Vector[BlockHeader] // 81+ bytes each
  ) extends MessagePayload
  
  case class GetAddrMessage() extends MessagePayload
  case class MempoolMessage() extends MessagePayload
  case class CheckOrderMessage() extends MessagePayload // deprecated
  case class SubmitOrderMessage() extends MessagePayload // deprecated
  case class ReplyMessage() extends MessagePayload // deprecated

  case class PingMessage(randomNonce: Long) extends MessagePayload
  case class PongMessage(nonce: Long) extends MessagePayload
  case class RejectMessage(
    message: String,
    ccode: Byte,
    reason: String,
    data: Vector[Byte]
  ) extends MessagePayload

*/
}
