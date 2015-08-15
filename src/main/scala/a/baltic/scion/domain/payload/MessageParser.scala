package a.baltic.scion.domain.payload

/**
 * @author andrew
 */
object MessageParser {

  private def parseLittleEndian(bytes: IndexedSeq[Byte], start: Int, n: Int): Option[(Long, Int)] = {
    if ((start + n) > bytes.length) {
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
          Some((bytes.slice(j, j + n.intValue()).mkString, j + n.intValue()))
        } else {
          None
        }
      }
    })
  }

}
