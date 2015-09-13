package a.baltic.scion

/**
 * @author andrew
 */
import a.baltic.scion.domain.payload.MessageParser
import scala.annotation.tailrec
import java.net.InetSocketAddress
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
  def hex(bs: IndexedSeq[Byte]): String = {
    bs.map("%02x" format _) .mkString
  }

  def unHex(h: String): IndexedSeq[Byte] = {
    h.grouped(2).map(Integer.parseInt(_, 16).toByte).toIndexedSeq
  }

  def networkAddressToBytes(a: InetSocketAddress): Vector[Byte] = {
    val bytes = a.getAddress.getAddress
    if (bytes.length == 16) {
      bytes.toVector
    } else {
      Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff).map(x => x.toByte) ++ bytes
    }
  }

}