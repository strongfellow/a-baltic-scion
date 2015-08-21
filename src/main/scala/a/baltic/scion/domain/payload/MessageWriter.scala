package a.baltic.scion.domain.payload

/**
 * @author andrew
 */
object MessageWriter {
  
  def writeInetAddress(a: java.net.InetAddress, port: Int): IndexedSeq[Byte] = {
    if (a.isSiteLocalAddress() || a.isLoopbackAddress()) {
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff, 0, 0, 0, 0, 0, 0).map(_.toByte)
    } else {
      a.getAddress ++ bigEndian(port, 2)
    }
  }
  
  def writeNetworkAddress(n: NetworkAddress, includeTimestamp: Boolean): IndexedSeq[Byte] = {
    n match {
      case NetworkAddress(
              timestamp: Option[Long], // maybe 4
              services: Long, // 8
              ip: java.net.InetAddress, // 16
              port: Int // 2
           ) => {
             ((if (includeTimestamp) littleEndian4(timestamp.get) else Vector()) 
                 ++ littleEndian8(services)
                 ++ writeInetAddress(ip, port))
           }
    }
  }

  def write(m: BitcoinMessageEnvelope): IndexedSeq[Byte] = {
    val command:String = m.payload match {
      case VersionMessage(_,_,_,_,_,_,_,_,_) => "version"
      case AddrMessage(_) => "addr"
      case VerackMessage() => "verack"
    }
    val payload = write(m.payload)
    (littleEndian4(m.magic)
        ++ command.getBytes ++ new Array[Byte](12 - command.length) 
        ++ littleEndian4(payload.length)
        ++ littleEndian4(a.baltic.scion.util.checksum(payload))
        ++ payload)
  }

  def write(m: BitcoinMessage): IndexedSeq[Byte] = {
    m match {
      case VersionMessage(
        version: Long, // 4 bytes
        services: Long, // 8 bytes
        timestamp: Long, // 8 bytes
        to: NetworkAddress, // 26
        from: NetworkAddress, // 26
        nonce: Long, // 8
        userAgent: String, // ?
        startHeight: Long, // 4
        relay: Boolean // 1
      ) => {
        (littleEndian4(version)
            ++ littleEndian8(services)
            ++ littleEndian8(timestamp)
            ++ writeNetworkAddress(to, false)
            ++ writeNetworkAddress(from, false)
            ++ littleEndian8(nonce)
            ++ writeVarString(userAgent)
            ++ littleEndian4(startHeight)
            ++ (if (version < 70001) { Vector() } else { Vector(if (relay) 1 else 0) }).map(_.toByte))
      }
      case AddrMessage(addresses) => {
        (writeVarInt(addresses.length)
          ++ addresses.flatMap { x => writeNetworkAddress(x, true) })
      }
      case VerackMessage() => Vector.empty
      case InvMessage(inventories) => {
        (writeVarInt(inventories.length)
          ++ inventories.flatMap { x => writeInventory(x) }
          )
      }
    }
  }
  
  def writeInventory(inv: Inventory) = {
    littleEndian(inv.invType, 4) ++ inv.hash
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
}
