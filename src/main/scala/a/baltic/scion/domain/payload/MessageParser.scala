package a.baltic.scion.domain.payload

import scala.annotation.tailrec

/**
 * @author andrew
 */
object MessageParser {

  def cmd(s:String): IndexedSeq[Byte] = {
    s.getBytes ++ Vector.fill(12 - s.length())(0.toByte)
  }

  val VERSION = cmd("version")
  val VERACK = cmd("verack")
  val ADDR = cmd("addr")

  def parseBitcoinMessage(
      bytes: IndexedSeq[Byte],
      start: Int,
      command: IndexedSeq[Byte],
      length: Long,
      checksum: Long): Option[(BitcoinMessage, Int)] = {
    
    val f = {
      command match {
        case VERSION => parseVersionMessage(bytes, start)
        case VERACK => Some((VerackMessage(), start))
        case ADDR => parseAddrMessage(bytes, start)
      }
    }
    
    if (a.baltic.scion.util.checksum(bytes.slice(start, start + length.intValue())) != checksum) {
      return None
    }
    val originalStart = start
    
    for {
      (m, s) <- f
      if (s - originalStart == length)
    } yield (m, s)
  }

  def parseBitcoinMessageEnvelope(
      bytes: IndexedSeq[Byte], start: Int): Option[(BitcoinMessageEnvelope, Int)] = {
    for {
      (magic, start) <- parseLittleEndian(bytes, start, 4)
      (command, start) <- parseCommand(bytes, start)
      (length, start) <- parseLittleEndian(bytes, start, 4)
      (checksum, start) <- parseLittleEndian(bytes, start, 4)
      (payload, start) <- parseBitcoinMessage(bytes, start, command, length, checksum)
    } yield (BitcoinMessageEnvelope(magic, payload), start)
  }

  def parseCommand(bytes: IndexedSeq[Byte], start: Int): Option[(IndexedSeq[Byte], Int)] = {
    if (start + 12 > bytes.length) {
      None
    } else {
      Some((bytes.slice(start, start + 12), start + 12))
    }
  }
  
  def parseBigEndian(bytes: IndexedSeq[Byte], start: Int, n: Int): Option[(Long, Int)] = {
    if ((start + n) > bytes.length) {
      None
    } else {
      val sum = bytes.slice(start, start + n).foldLeft(0){(a, b) => (a << 8) + (b & 0xff)}
      Some((sum, start + n))
    }
  }
  
  def parseLittleEndian(bytes: IndexedSeq[Byte], start: Int, n: Int): Option[(Long, Int)] = {
    if ((start + n) > bytes.length) {
      println("start: " + start)
      println("n:     " + n)
      println("bytes.length: " + bytes.length)
      None
    } else {
      val sum = (0 until n).map {
        i:Int => (bytes(start + i) & 0xffL) << (8 * i)
      }.sum
      Some((sum, start + n))
    }
  }

  def parseVarInt(bytes: IndexedSeq[Byte], start: Int): (Option[(Long, Int)]) = {
    if (start < bytes.length) {
      val b: Int = bytes(start) & 0xff
      if (b < 0xfd) {
        Some((b, start + 1))
      } else if (b == 0xfd) {
        parseLittleEndian(bytes, start + 1,2)
      } else if (b == 0xfe) {
        parseLittleEndian(bytes, start + 1, 4)
      } else {
        parseLittleEndian(bytes, start + 1, 8)
      }
    } else {
      None
    }
  }
  
  def parseString(bytes: IndexedSeq[Byte], i: Int): Option[(String, Int)] = {
    parseVarInt(bytes, i).flatMap({
      case (n: Long, j: Int) => {
        if (j + n <= bytes.length) {
          Some((bytes.slice(j, j + n.intValue()).map(_.toChar).mkString, j + n.intValue()))
        } else {
          None
        }
      }
    })
  }

  def parseHash(bytes: IndexedSeq[Byte], start: Int): Option[(IndexedSeq[Byte], Int)] = {
    if (start + 32 > bytes.length) {
      None
    } else {
      Some((bytes.slice(start, start + 32), start + 32))
    }
  }

  def parseNetworkAddress(
      bytes: IndexedSeq[Byte],
      start: Int,
      includeTimestamp: Boolean): Option[(NetworkAddress, Int)] = {
    
    def parseTimestamp(bytes: IndexedSeq[Byte], start: Int, includeTimestamp: Boolean): Option[(Option[Long], Int)] = {
      if (includeTimestamp) {
        for {
          (n, i) <- parseLittleEndian(bytes, start, 4)
        } yield (Some(n), i)
      } else {
        Some((None, start))
      }
    }
    
    def parseIp(bytes: IndexedSeq[Byte], start: Int): Option[(java.net.InetAddress, Int)] = {
      if (start + 16 > bytes.length) {
        None
      } else {
        val bs: Array[Byte] = bytes.slice(start, start + 16).toArray
        Some((java.net.InetAddress.getByAddress(bs), start + 16))
      }
    }
    for {
      (timestamp, i) <- parseTimestamp(bytes, start, includeTimestamp)
      (services, j) <- parseLittleEndian(bytes, i, 8)
      (ip, k) <- parseIp(bytes, j)
      (port, l) <- parseBigEndian(bytes, k, 2)
    } yield (NetworkAddress(timestamp, services, ip, port.intValue()), l)
  }

  def parseAddrMessage(bytes: IndexedSeq[Byte], start: Int): Option[(AddrMessage, Int)] = {
    for {(as, x) <- varTimes((bs: IndexedSeq[Byte], s: Int) => parseNetworkAddress(bs, s, true), 
                   bytes,
                   start)
         } yield (AddrMessage(as), x)
  }

  def parseVersionMessage(bytes: IndexedSeq[Byte], start: Int): Option[(VersionMessage, Int)] = {
/**
 * 4  version   int32_t   Identifies protocol version being used by the node
8   services  uint64_t  bitfield of features to be enabled for this connection
8   timestamp   int64_t   standard UNIX timestamp in seconds
26  addr_recv   net_addr  The network address of the node receiving this message
Fields below require version ≥ 106
26  addr_from   net_addr  The network address of the node emitting this message
8   nonce   uint64_t  Node random nonce, randomly generated every time a version packet is sent. This nonce is used to detect connections to self.
 ?  user_agent  var_str   User Agent (0x00 if string is 0 bytes long)
4   start_height  int32_t   The last block received by the emitting node
Fields below require version ≥ 70001
1   relay   bool  Whether the remote peer should announce relayed transactions or not, see BIP 0037
 */
    def parseBoolean(version: Long, bytes: IndexedSeq[Byte], start: Int): Option[(Boolean, Int)] = {
      if (version >= 70001L) {
        for {
          (b, i) <- parseLittleEndian(bytes, start, 1)
        } yield ((b != 0), i)
      } else {
        Some((true, start))
      }
    }
    for {
      (version, i) <- parseLittleEndian(bytes, start, 4)
      (services, j) <- parseLittleEndian(bytes, i, 8)
      (timestamp, k) <- parseLittleEndian(bytes, j, 8)
      (receiver, l) <- parseNetworkAddress(bytes, k, false)
      (sender, m) <- parseNetworkAddress(bytes, l, false)
      (nonce, n) <- parseLittleEndian(bytes, m, 8)
      (userAgent, o) <- parseString(bytes, n)
      (startHeight, p) <- parseLittleEndian(bytes, o, 4)
      (relay, q) <- parseBoolean(version, bytes, p)
    } yield (VersionMessage(
      version,
      services,
      timestamp,
      receiver,
      sender,
      nonce,
      userAgent,
      startHeight,
      relay
    ), q)
  }

  def varTimes[T](f: (IndexedSeq[Byte], Int) => Option[(T, Int)],
                  bytes: IndexedSeq[Byte],
                  start: Int) = {
    @tailrec def n(start: Int, ts: Vector[T], remaining: Int): Option[(Vector[T], Int)] = {
      if (remaining <= 0) {
        Some((ts, start))
      } else {
        val x = f(bytes, start)
        x match {
          case None => None
          case Some((t, s)) => return n(s, ts :+ t, remaining - 1)
        }
      }
    }
    for {
      (nTimes, s) <- parseVarInt(bytes, start)
      x <- n(s, Vector.empty[T], nTimes.intValue())
    } yield x
  }
  
}
