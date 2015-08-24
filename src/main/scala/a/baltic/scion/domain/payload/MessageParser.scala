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
  val INV = cmd("inv")
  val GETDATA = cmd("getdata")
  val NOTFOUND = cmd("notfound")
  val GETBLOCKS = cmd("getblocks")
  val GETHEADERS = cmd("getheaders")
  val TX = cmd("tx")
  val BLOCK = cmd("block")
  val HEADERS = cmd("headers")
  val GETADDR = cmd("getaddr")
  val MEMPOOL = cmd("mempool")
  val CHECKORDER = cmd("checkorder")
  val SUBMITORDER = cmd("submitorder")
  val REPLY = cmd("reply")
  val PING = cmd("ping")
  val PONG = cmd("pong")
  val REJECT = cmd("reject")
  val FILTERLOAD = cmd("filterload")
  val FILTERADD = cmd("filteradd")
  val FILTERCLEAR = cmd("filterclear")
  val MERKLEBOCK = cmd("merklebock")
  val ALERT = cmd("alert")

  def parseBitcoinMessage(
      bytes: IndexedSeq[Byte],
      start: Int,
      command: IndexedSeq[Byte],
      length: Long,
      checksum: Long): Parser[BitcoinMessage] = {
    
    val f = {
      command match {
        case VERSION => parseVersionMessage(bytes, start)
        case VERACK => Some((VerackMessage(), start))
        case ADDR => parseAddrMessage(bytes, start)
        case INV => parseInvMessage(bytes, start)
        case GETDATA => parseGetDataMessage(bytes, start)
        case NOTFOUND => parseNotFoundMessage(bytes, start)
        case GETBLOCKS => parseGetBlocksMessage(bytes, start)
        case GETHEADERS => parseGetHeadersMessage(bytes, start)
        case TX => parseTxMessage(bytes, start)
        case BLOCK => parseBlockMessage(bytes, start)
        case HEADERS => parseHeadersMessage(bytes, start)
        case GETADDR => parseGetAddrMessage(bytes, start)
        case MEMPOOL => parseMemPoolMessae(bytes, start)
        case CHECKORDER => parseCheckOrderMessage(bytes, start)
        case SUBMITORDER => parseSubmitOrderMessage(bytes, start)
        case REPLY => parseReplyMessage(bytes, start)
        case PING => parsePingMessage(bytes, start)
        case PONG => parsePongMessage(bytes, start)
        case REJECT => parseRejectMessage(bytes, start, start + length.intValue())
        case FILTERLOAD => parseFilterLoadMessage(bytes, start)
        case FILTERADD => parseFilterAddMessage(bytes, start)
        case FILTERCLEAR => parseFilterClearMessage(bytes, start)
        case MERKLEBOCK => parseMerkleBlockMessage(bytes, start)
        case ALERT => parseAlertMessage(bytes, start)
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
      bytes: IndexedSeq[Byte], start: Int): Parser[BitcoinMessageEnvelope] = {
    for {
      (magic, start) <- parseLittleEndian(bytes, start, 4)
      (command, start) <- parseCommand(bytes, start)
      (length, start) <- parseLittleEndian(bytes, start, 4)
      (checksum, start) <- parseLittleEndian(bytes, start, 4)
      (payload, start) <- parseBitcoinMessage(bytes, start, command, length, checksum)
    } yield (BitcoinMessageEnvelope(magic, payload), start)
  }

  def parseCommand(bytes: IndexedSeq[Byte], start: Int): Parser[IndexedSeq[Byte]] = {
    nBytes(bytes, start, 12)
  }
  
  def parseBigEndian(bytes: IndexedSeq[Byte], start: Int, n: Int): Parser[Long] = {
    if ((start + n) > bytes.length) {
      None
    } else {
      val sum = bytes.slice(start, start + n).foldLeft(0){(a, b) => (a << 8) + (b & 0xff)}
      Some((sum, start + n))
    }
  }
  
  def parseLittleEndian(bytes: IndexedSeq[Byte], start: Int, n: Int): Parser[Long] = {
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

  def parseVarInt(bytes: IndexedSeq[Byte], start: Int): Parser[Long] = {
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
  
  def parseString(bytes: IndexedSeq[Byte], start: Int): Parser[String] = {
    for {
      (n, start) <- parseVarInt(bytes, start)
      (bs, start) <- nBytes(bytes, start, n)
    } yield (bs.map(_.toChar).mkString, start)
  }

  def parseHash(bytes: IndexedSeq[Byte], start: Int): Parser[Hash] = {
    nBytes(bytes, start, 32)
  }
  
  def parseNetworkAddress(
      bytes: IndexedSeq[Byte],
      start: Int,
      includeTimestamp: Boolean): Parser[NetworkAddress] = {
    
    def parseTimestamp(bytes: IndexedSeq[Byte], 
        start: Int,
        includeTimestamp: Boolean): Parser[Option[Long]] = {
      if (includeTimestamp) {
        for {
          (n, i) <- parseLittleEndian(bytes, start, 4)
        } yield (Some(n), i)
      } else {
        Some((None, start))
      }
    }
    
    def parseIp(bytes: IndexedSeq[Byte], start: Int): Parser[java.net.InetAddress] = {
      for {
        (bs, start) <- nBytes(bytes, start, 16)
      } yield (java.net.InetAddress.getByAddress(bs.toArray), start)
    }
    for {
      (timestamp, i) <- parseTimestamp(bytes, start, includeTimestamp)
      (services, j) <- parseLittleEndian(bytes, i, 8)
      (ip, k) <- parseIp(bytes, j)
      (port, l) <- parseBigEndian(bytes, k, 2)
    } yield (NetworkAddress(timestamp, services, ip, port.intValue()), l)
  }

  def parseAddrMessage(bytes: IndexedSeq[Byte], start: Int): Parser[AddrMessage] = {
    for {(as, x) <- varTimes((bs: IndexedSeq[Byte], s: Int) => parseNetworkAddress(bs, s, true), 
                   bytes,
                   start)
         } yield (AddrMessage(as), x)
  }
  
  def parseInventoryVector(bs: IndexedSeq[Byte], s:Int) = {
    for {
      (invType, s) <- parseLittleEndian(bs, s, 4)
      (hash, s) <- parseHash(bs, s)
    } yield (Inventory(invType, hash), s)
  }
  
  private def parseGetDataMessage(bytes: IndexedSeq[Byte], start: Int) = {
    for {
      (as, x) <- varTimes(parseInventoryVector, bytes, start)
    } yield (GetDataMessage(as), x)
  }
  
  private def parseNotFoundMessage(bytes: IndexedSeq[Byte], start: Int) = {
    for {
      (as, x) <- varTimes(parseInventoryVector, bytes, start)
    } yield (NotFoundMessage(as), x)
  }
  
 private def parseInvMessage(bytes: IndexedSeq[Byte], start: Int) = {
    for {
      (as, x) <- varTimes(parseInventoryVector, bytes, start)
    } yield (InvMessage(as), x)
  }

  def parseVersionMessage(bytes: IndexedSeq[Byte], start: Int): Parser[VersionMessage] = {
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
    def parseRelayBoolean(version: Long, bytes: IndexedSeq[Byte], start: Int): Parser[Boolean] = {
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
      (relay, q) <- parseRelayBoolean(version, bytes, p)
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

  def varTimes[T](f: (IndexedSeq[Byte], Int) => Parser[T],
                  bytes: IndexedSeq[Byte],
                  start: Int) = {
    @tailrec def n(start: Int, ts: Vector[T], remaining: Int): Parser[Vector[T]] = {
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

  private def parseGetBlocksMessage(bytes: IndexedSeq[Byte],
      start: Int): Parser[GetBlocksMessage] = {
    for {
      (version, start) <- parseLittleEndian(bytes, start, 4)
      (hashes, start) <- varTimes(parseHash, bytes, start)
      (hashStop, start) <- parseHash(bytes, start)
    } yield (GetBlocksMessage(version, hashes, hashStop), start)
  }

  private def parseGetHeadersMessage(bytes: IndexedSeq[Byte], start: Int) = {
for {
      (version, start) <- parseLittleEndian(bytes, start, 4)
      (hashes, start) <- varTimes(parseHash, bytes, start)
      (hashStop, start) <- parseHash(bytes, start)
    } yield (GetHeadersMessage(version, hashes, hashStop), start)
  }
  
  def parseScript(bytes: IndexedSeq[Byte], start: Int): Parser[Script] = {
    for {
      (n, start) <- parseVarInt(bytes, start)
      x <- nBytes(bytes, start, n)
    } yield x
  }
  
  private def nBytes(bytes: IndexedSeq[Byte], start: Int, n: Long): Parser[Vector[Byte]] = {
    if (start + n.intValue() <= bytes.length) {
      Some(bytes.slice(start, start + n.intValue()).toVector, start + n.intValue())
    } else {
      None
    }
  }

  private def parseTxin(bytes: IndexedSeq[Byte], start: Int) = {
    for {
      (hash, start) <- parseHash(bytes, start)
      (index, start) <- parseLittleEndian(bytes, start, 4)
      (script, start) <- parseScript(bytes, start)
      (sequence, start) <- parseLittleEndian(bytes, start, 4)
    } yield (TxIn(hash, index, script, sequence), start)
  }
  private def parseTxout(bytes: IndexedSeq[Byte], start: Int) = {
    for {
      (value, start) <- parseLittleEndian(bytes, start, 8)
      (script, start) <- parseScript(bytes, start)
    } yield (TxOut(value, script), start)
  }
  
  private def parseVersion(bytes: IndexedSeq[Byte], start: Int) = {
    parseLittleEndian(bytes, start, 4) 
  }
  private def parseTxMessage(bytes: IndexedSeq[Byte], start: Int) = {
    for {
      (version, start) <- parseVersion(bytes, start)
      (txins, start) <- varTimes(parseTxin, bytes, start)
      (txouts, start) <- varTimes(parseTxout, bytes, start)
      (lockTime, start) <- parseLittleEndian(bytes, start, 4)
    } yield (TxMessage(version, txins, txouts, lockTime), start)
  }
  
  private def parseBlockHeader(bytes: IndexedSeq[Byte], start: Int) = {
    for {
      (version, start) <- parseVersion(bytes, start)
      (previousBlock, start) <- parseHash(bytes, start)
      (merkleRoot, start) <- parseHash(bytes, start)
      (timestamp, start) <- parseLittleEndian(bytes, start, 4)
      (bits, start) <- parseLittleEndian(bytes, start, 4)
      (nonce, start) <- parseLittleEndian(bytes, start, 4)
    } yield (BlockHeader(version, previousBlock, merkleRoot, timestamp, bits, nonce), start)
  }
  
  private def parseBlockMessage(bytes: IndexedSeq[Byte], start: Int) = {
    for {
      (header, start) <- parseBlockHeader(bytes, start)
      (transactions, start) <- varTimes(parseTxMessage, bytes, start)
    } yield (BlockMessage(header, transactions), start)
  }

  private def parseHeadersMessage(bytes: IndexedSeq[Byte], start: Int) = {
    for {
      (blocks, start) <-varTimes(parseBlockMessage, bytes, start)
    } yield (HeadersMessage(blocks), start)
  }

  private def parseGetAddrMessage(bytes: IndexedSeq[Byte], start: Int) = {
    Some(GetAddrMessage(), start)
  }
  private def parseMemPoolMessae(bytes: IndexedSeq[Byte], start: Int) = {
    Some(MemPoolMessage(), start)
  }

  private def parseCheckOrderMessage(bytes: IndexedSeq[Byte], start: Int) = {
    None
  }

  private def parseSubmitOrderMessage(bytes: IndexedSeq[Byte], start: Int) = {
    None
  }
  private def parseReplyMessage(bytes: IndexedSeq[Byte], start: Int): Parser[ReplyMessage] = {
    None
  }
  private def parsePingMessage(bytes: IndexedSeq[Byte], start: Int): Parser[PingMessage] = {
    for {
      (nonce, start) <- parseLittleEndian(bytes, start, 8)
    } yield (PingMessage(nonce), start)
  }

  private def parsePongMessage(bytes: IndexedSeq[Byte], start: Int):  Parser[PongMessage] = {
    for {
      (nonce, start) <- parseLittleEndian(bytes, start, 8)
    } yield (PongMessage(nonce), start)
  }

  private def parseRejectMessage(bytes: IndexedSeq[Byte], start: Int, end: Int): Parser[RejectMessage] = {
    for {
      (message, start) <- parseString(bytes, start)
      (ccode, start) <- Some(bytes(start), start + 1)
      (reason, start) <- parseString(bytes, start)
      (data, start) <- nBytes(bytes, start, (end - start))
    } yield (RejectMessage(message, ccode, reason, data), start)
  }

  private def parseFilterLoadMessage(bytes: IndexedSeq[Byte], start: Int): Parser[FilterLoadMessage] = {
    None
  }
  private def parseFilterAddMessage(bytes: IndexedSeq[Byte], start: Int): Parser[FilterAddMessage] = {
    None
  }
  private def parseFilterClearMessage(bytes: IndexedSeq[Byte], start: Int): Parser[FilterClearMessage] = {
    None
  }
  private def parseMerkleBlockMessage(bytes: IndexedSeq[Byte], start: Int): Parser[MerkleBlockMessage] = {
    None
  }
  private def parseAlertMessage(bytes: IndexedSeq[Byte], start: Int): Parser[AlertMessage] = {
    None
  }
  
}
