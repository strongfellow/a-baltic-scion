package a.baltic.scion

/**
 * @author andrew
 */
package object util {
  import java.security.MessageDigest
  import org.bouncycastle.jcajce.provider.digest.SHA256.Digest 

  def checksum(bs: Seq[Byte]): Long = {
    parseLittleEndian(doubleSHA256(bs).take(4))
  }

  def doubleSHA256(bs: Seq[Byte])  = {
    val md = new Digest
    md.update(bs.toArray)
    val h1 = md.digest()
    md.reset
    md.update(h1)
    md.digest()
  }
  def hex(bs: Seq[Byte]): String = {
    bs.map("%02x" format _) .mkString
  }

  def unHex(h: String): Seq[Byte] = {
    h.grouped(2).map(Integer.parseInt(_, 16).toByte).toSeq
  }



  def parseVarInt(bs: Seq[Byte]): Option[Long] = {
    if (bs.isEmpty) {
      None
    } else {
      val x = bs.head & 0xff
      x match {
        case 0xfd => parseLittleEndian(bs.drop(1), 2)
        case 0xfe => parseLittleEndian(bs.drop(1), 4)
        case 0xff => parseLittleEndian(bs.drop(1), 8)
        case _    => Some(x)
      }
    }
  }

  def parseLittleEndian(bs:Seq[Byte], n: Int): Option[Long] = {
    if (bs.length < n) {
      None
    } else {
      Some(parseLittleEndian(bs.take(n)))
    }
  }

  private def parseLittleEndian(bs:Seq[Byte]): Long = {
    bs.zipWithIndex.map {
      case (v, i) => ((v & 0xffL) << (8 * i))
    }.sum
  }

}