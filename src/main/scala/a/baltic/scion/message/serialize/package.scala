package a.baltic.scion.message

import a.baltic.scion.util

/**
 * @author andrew
 */
import akka.util.ByteStringBuilder
package object serialize {
  
  
  /**
  def serializeMessage(m: Message): Seq[Byte] = {
    val magic = m.magic
    val command = m.command
    val payload = m.payload
    val result = new ByteStringBuilder()
    result ++= util.littleEndian4(magic)
    result ++= command.getBytes
    result ++= new Array[Byte](12 - command.length)
    result ++= util.littleEndian4(payload.length)
    result ++= util.checksum(payload)
    result ++= payload
    result.result()
  }

  def parseMessage(s: Seq[Byte]): Option[Message] = {
    val magic = util.parseLittleEndian(s, 4)
    val command = parseCommand(s.drop(4))
    val expectedLength = util.parseLittleEndian(s.drop(16), 4)
    val expectedChecksum = s.drop(20).take(4)
    
    val payload = expectedLength.map { x =>
      s.drop(24).take(x.intValue())
    }
    for {
      m <- magic
      c <- command
      l <- expectedLength
      p <- payload if (p.length == l && util.checksum(p).sameElements(expectedChecksum))
    } yield Message(m, c, p)
  }
  * 
  */

}
