package a.baltic.scion.domain

import a.baltic.scion.util.checksum

/**
 * @author andrew
 */
package object payload {

  type Script = Vector[Byte]
  type Hash = Vector[Byte]
  type Parser[T] = Option[(T, Int)]

  case class BitcoinMessageEnvelope(
    magic: Long,
    payload: BitcoinMessage
  )

  trait Command {
    def command: String
  }

  trait BitcoinSerializable {
    def serialize(): IndexedSeq[Byte]
  }
  sealed abstract class BitcoinMessage extends Command with BitcoinSerializable
  sealed abstract class EmptyMessage extends BitcoinMessage {
    final def serialize() = { Vector.empty}
  }
  
  case class Inventory(
    invType: Long, // 4 bytes
    hash: Hash // 32 bytes
  ) extends BitcoinSerializable {
    def serialize() = {
      MessageWriter.littleEndian4(invType) ++ hash
    }
  }

  case class TxIn(
      hash: Hash,
      index: Long,
      script: Script,
      sequence: Long
  ) extends BitcoinSerializable {
    def serialize() = {
      (hash
          ++ MessageWriter.littleEndian4(index)
          ++ script
          ++ MessageWriter.littleEndian4(sequence))
    }
  }
  
  case class TxOut(
      value: Long,
      script: Script
  ) extends BitcoinSerializable {
    def serialize() = {
      MessageWriter.littleEndian8(value) ++ script
    }
  }
  
  case class NetworkAddress(
    timestamp: Option[Long], // maybe 4
    services: Long, // 8
    ip: java.net.InetAddress, // 16
    port: Int // 2
  ) extends BitcoinSerializable {
    def serialize() = {
      ((timestamp match {
        case Some(x) => MessageWriter.littleEndian4(x)
        case None => Vector.empty
       })
         ++ MessageWriter.littleEndian8(services)
         ++ MessageWriter.writeInetAddress(ip, port))
    }
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
  ) extends BitcoinMessage {
    val command = "version"
    def serialize() = {
      (MessageWriter.littleEndian4(version)
          ++ MessageWriter.littleEndian8(services)
          ++ MessageWriter.littleEndian8(timestamp)
          ++ MessageWriter.writeNetworkAddress(to, false)
          ++ MessageWriter.writeNetworkAddress(from, false)
          ++ MessageWriter.littleEndian8(nonce)
          ++ MessageWriter.writeVarString(userAgent)
          ++ MessageWriter.littleEndian4(startHeight)
          ++ (if (version > 70000) Vector(if (relay) 1 else 0) else Vector.empty).map(_.toByte)
      )
    }
  }

  case class VerackMessage() extends BitcoinMessage {
    val command = "verack"
    def serialize() = Vector.empty[Byte]
  }
  
  case class AddrMessage(
      addresses: Vector[NetworkAddress]
  ) extends BitcoinMessage {
    val command = "addr"
    def serialize() = {
      MessageWriter.serializeVector(addresses)
    }
  }

  case class InvMessage(
    inventories: Vector[Inventory]
  ) extends BitcoinMessage {
    val command = "inv"
    def serialize() = {
      MessageWriter.serializeVector(inventories)
    }
  }
  
  case class GetDataMessage(
    inventories: Vector[Inventory]
  ) extends BitcoinMessage {
    val command = "getdata"
    def serialize() = {
      MessageWriter.serializeVector(inventories)
    }
  }
  
  case class NotFoundMessage(
      inventories: Vector[Inventory]
  ) extends BitcoinMessage {
    val command = "notfound"
    def serialize() = {
      MessageWriter.serializeVector(inventories)
    }
  }
  
  case class GetBlocksMessage(
      version: Long,
      hashes: Vector[Hash],
      hashStop: Hash
  ) extends BitcoinMessage {
    val command = "getblocks"
    def serialize() = {
      (MessageWriter.littleEndian4(version)
          ++ MessageWriter.writeHashes(hashes)
          ++ hashStop)
    }
  }
  
  case class GetHeadersMessage(
      version: Long,
      hashes: Vector[Hash],
      hashStop: Hash
  ) extends BitcoinMessage {
    val command = "getheaders"
    def serialize() = {
      (MessageWriter.littleEndian4(version)
          ++ MessageWriter.writeHashes(hashes)
          ++ hashStop)
    }
  }
  
  case class TxMessage(
      version: Long,
      txins: Vector[TxIn],
      txouts: Vector[TxOut],
      lockTime: Long
  ) extends BitcoinMessage {
    val command = "tx"
    def serialize() = {
      (MessageWriter.littleEndian4(version)
          ++ MessageWriter.serializeVector(txins)
          ++ MessageWriter.serializeVector(txouts)
          ++ MessageWriter.littleEndian4(lockTime))
    }
  }

  case class BlockMessage(
      header: BlockHeader,
      transactions: Vector[TxMessage]
  ) extends BitcoinMessage {
    val command = "block"
    def serialize() = {
      header.serialize() ++ MessageWriter.serializeVector(transactions)
    }
  }

  case class HeadersMessage(
      headers: Vector[BlockMessage]
  ) extends BitcoinMessage {
    val command = "headers"
    def serialize() = {
      MessageWriter.serializeVector(headers)
    }
  }

  case class GetAddrMessage() extends EmptyMessage { val command = "getaddr" }
  case class MemPoolMessage() extends EmptyMessage { val command = "mempool" }

  case class CheckOrderMessage() extends EmptyMessage { val command = "checkorder" }
  case class SubmitOrderMessage() extends EmptyMessage { val command = "submitorder" }
  case class ReplyMessage() extends EmptyMessage { val command = "reply" }

  case class PingMessage(nonce: Long) extends BitcoinMessage {
    val command = "ping"
    def serialize() = {
      MessageWriter.littleEndian8(nonce)
    }
  }

  case class PongMessage(nonce: Long) extends BitcoinMessage {
    val command = "pong"
    def serialize() = {
      MessageWriter.littleEndian8(nonce)
    }
  }

  case class RejectMessage(
      message: String,
      ccode: Int,
      reason: String,
      data: Vector[Byte]
  ) extends BitcoinMessage {
    val command = "reject"
    def serialize() = {
      (MessageWriter.writeVarString(message)
          ++ Vector(ccode.toByte)
          ++ MessageWriter.writeVarString(reason)
          ++ data)
    }
  }

  case class FilterLoadMessage(
      filter: Vector[Byte],
      numberOfHashFunctions: Long,
      nTweak: Long,
      nFlags: Byte
  ) extends BitcoinMessage {
    val command = "filterload"
    def serialize() = {
      (MessageWriter.writeBytes(filter)
          ++ MessageWriter.littleEndian4(numberOfHashFunctions)
          ++ MessageWriter.littleEndian4(nTweak)
          ++ Vector(nFlags))
    }
  }

  case class FilterAddMessage(
      data: Vector[Byte]
  ) extends BitcoinMessage {
    val command = "filteradd"
    def serialize() = {
      MessageWriter.writeBytes(data)
    }
  }

  final case class BlockHeader(
      version: Long,
      previousBlock: Hash,
      merkleRoot: Hash,
      timestamp: Long,
      bits: Long,
      nonce: Long) extends BitcoinSerializable {
    def serialize() = {
            (MessageWriter.littleEndian4(version) // 4 bytes
          ++ previousBlock // 32 bytes
          ++ merkleRoot // 32 bytes
          ++ MessageWriter.littleEndian4(timestamp) // 4 bytes
          ++ MessageWriter.littleEndian4(bits) // 4 bytes
          ++ MessageWriter.littleEndian4(nonce)) // 4 bytes
    }
  }
    

  
  case class FilterClearMessage() extends EmptyMessage { val command = "filterclear" }
  case class MerkleBlockMessage(
      header: BlockHeader,
      numTx: Long,
      hashes: Vector[Hash],
      flags: Vector[Byte]
  ) extends BitcoinMessage {
    val command = "merkleblock"
    def serialize() = {
      (header.serialize()
          ++ MessageWriter.littleEndian4(numTx)
          ++ MessageWriter.writeHashes(hashes)
          ++ MessageWriter.writeBytes(flags))
    }
  }
  case class AlertMessage(
      payload: Vector[Byte],
      signature: Vector[Byte]
  ) extends BitcoinMessage {
    val command = "alert"
    def serialize() = {
      MessageWriter.writeBytes(payload) ++ MessageWriter.writeBytes(payload)
    }
  }

}
