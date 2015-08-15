package a.baltic.scion.domain.payload

/**
 * @author andrew
 */
object MessageWriter {
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
    ((bytes - 1) to 0) map {
      i => ((n >>> (8 * i)) & 0xff).toByte
    }
  }
}
