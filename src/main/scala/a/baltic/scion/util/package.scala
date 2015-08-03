package a.baltic.scion

/**
 * @author andrew
 */
package object util {
  import java.security.MessageDigest
  import org.bouncycastle.jcajce.provider.digest.SHA256.Digest 

  def checksum(bs: Seq[Byte]) = doubleSHA256(bs).take(4)

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

  def varInt(n: Long):Seq[Byte] = {
    if (n > -1) {
      if (n < 0xfdL) {
        Seq(n.toByte)
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

  def varInt(bs: Seq[Byte]): Long = {
    val x = bs.head & 0xff
    if (x < 0xfd) {
      x
    } else {
      val len = if (x == 0xfd) { 2 } else if (x == 0xfe) { 4 } else { 8 }
      littleEndian(bs.drop(1).take(len))
    }
  }

  def littleEndian(bs:Seq[Byte]): Long = {
    bs.zipWithIndex.map {
      case (v, i) => ((v & 0xffL) << (8 * i))
    }.sum
  }

  def varString(s: String): Seq[Byte] = {
    varInt(s.length()) ++ s.map { _.toByte }
  }

  def network2(n: Long) = bigEndian(n, 2)
  def littleEndian4(n: Long) = littleEndian(n, 4)
  def littleEndian8(n: Long) = littleEndian(n, 8)

  private def littleEndian(n: Long, bytes: Int):Seq[Byte] = {
    Vector.iterate(n, bytes) { _ >> 8 } map { _.toByte }
  }

  private def bigEndian(n: Long, bytes: Int): Seq[Byte] = {
    ((bytes - 1) to 0) map {
      i => ((n >>> (8 * i)) & 0xff).toByte
    }
  }

}