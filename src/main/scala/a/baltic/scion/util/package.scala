package a.baltic.scion

/**
 * @author andrew
 */
import a.baltic.scion.domain.payload.MessageParser
package object util {
  import java.security.MessageDigest
  import org.bouncycastle.jcajce.provider.digest.SHA256.Digest 

  def checksum(bs: Seq[Byte]): Long = {
    MessageParser.parseLittleEndian(doubleSHA256(bs), 0, 4).get._1
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

}