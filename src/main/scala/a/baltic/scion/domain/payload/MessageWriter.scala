
package a.baltic.scion.domain.payload

/**
 * @author andrew
 */
object MessageWriter {

  def writeInetAddress(a: Vector[Byte], port: Int): IndexedSeq[Byte] = {
    val address = java.net.InetAddress.getByAddress(a.toArray)
    if (address.isSiteLocalAddress() || address.isLoopbackAddress()) {
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff, 0, 0, 0, 0, 0, 0).map(_.toByte)
    } else {
      val addr = a
      (if (addr.length == 4) Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff) else Vector()
          ).map {_.toByte } ++ addr ++ bigEndian(port, 2)
    }
  }

  def writeNetworkAddress(n: NetworkAddress, includeTimestamp: Boolean): IndexedSeq[Byte] = {
    n match {
      case NetworkAddress(
              timestamp: Option[Long], // maybe 4
              services: Long, // 8
              ip: Vector[Byte],
              port: Int // 2
           ) => {
             val ts = (if (includeTimestamp) littleEndian4(timestamp.get) else Vector())
             val svc = littleEndian8(services)
             val inet = writeInetAddress(ip, port)
             ts ++ svc ++ inet
           }
    }
  }

  def write(m: BitcoinMessageEnvelope): IndexedSeq[Byte] = {
    val command:String = m.payload.command
    val payload = m.payload.serialize
    (littleEndian4(m.magic)
        ++ command.getBytes ++ new Array[Byte](12 - command.length)
        ++ littleEndian4(payload.length)
        ++ littleEndian4(a.baltic.scion.util.checksum(payload))
        ++ payload)
  }

  def writeVarInt(n: Long): IndexedSeq[Byte] = {
    if (n > -1) {
      if (n < 0xfdL) {
        Vector(n.toByte)
      } else if (n <= 0xffffL) {
        0xfd.toByte +: littleEndian(n, 2)
      } else if (n <= 0xffffffffL) {
        0xfe.toByte +: littleEndian(n, 4)
      } else {
        0xff.toByte +: littleEndian(n, 8)
      }
    } else {
      0xff.toByte +: littleEndian(n, 8)
    }
  }

  def writeHashes(hashes: Vector[Vector[Byte]]) = {
    writeVarInt(hashes.length) ++ hashes.flatMap((x:Vector[Byte]) => x)
  }

  def writeBytes(bs: Vector[Byte]) = {
    writeVarInt(bs.length) ++ bs
  }

  def writeVarString(s: String): IndexedSeq[Byte] = {
    writeVarInt(s.length()) ++ s.map { _.toByte }
  }
  def network2(n: Long) = bigEndian(n, 2)
  def littleEndian4(n: Long) = littleEndian(n, 4)
  def littleEndian8(n: Long) = littleEndian(n, 8)
  private def littleEndian(n: Long, bytes: Int): IndexedSeq[Byte] = {
    Vector.iterate(n, bytes) { _ >> 8 } map { _.toByte }
  }
  private def bigEndian(n: Long, bytes: Int): IndexedSeq[Byte] = {
    val bs = (1 to bytes) map {
      i => ((n >>> (8 * (bytes - i))) & 0xff).toByte
    }
    bs
  }

  def serializeVector(items:Vector[BitcoinSerializable]) = {
    writeVarInt(items.length) ++ items.flatMap(_.serialize())
  }
}
