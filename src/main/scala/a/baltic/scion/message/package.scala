package a.baltic.scion

/**
 * @author andrew
 */
import akka.util.ByteString
import akka.util.ByteString.ByteString1
import akka.util.ByteStringBuilder
package object message {
/**
  case class Message(magic: Long, command: String, payload: Seq[Byte])
  sealed abstract class Payload {}
  case class VersionPayload(
  ) extends Payload

  def parseCommand(bs: Seq[Byte]): Option[String] = {
    if (bs.length < 12) {
      None
    } else {
      val cmd = bs.take(12)
      if (cmd.dropWhile(_ != 0).exists(_ != 0)) {
        None
      } else {
        Some(cmd.takeWhile(_ != 0).map(_.toChar).mkString(""))
      }
    }
  }
  
  val MAIN:Array[Byte] = Array(0xF9, 0xBE, 0xB4, 0xD9).map(_.toByte)
  
  def m(s:String):Array[Byte] = s.getBytes ++ new Array[Byte](12 - s.length)
  
  val VERACK = m("verack")


  def versionMessage(
    version: Long,
    services: Long,
    timestamp: Long,
    receiver: NetworkAddress,
    emitter: NetworkAddress,
    nonce: Long,
    userAgent: String,
    startHeight: Long
//    relay: Boolean // TODO: add me back in
  ) = {
    val payload = new ByteStringBuilder()
    payload ++= util.littleEndian4(version)
    payload ++= util.littleEndian8(services)
    payload ++= util.littleEndian8(timestamp)

    netAddr(payload, services, receiver)
    netAddr(payload, services, emitter)
    payload ++= util.littleEndian8(nonce)
    payload ++= util.varString(userAgent)
    payload ++= util.littleEndian4(startHeight)
//    payload ++= Seq((if (relay) 1 else 0).toByte) // todo: ADD me back in
    message(MAIN, "version", payload.result())
  }

  private def isAddressable(ipAddress: Array[Byte]): Boolean = {
    if (ipAddress.length == 4) {
      if (ipAddress(0) == 127) {
        false
      } else {
        true
      }
    } else {
      true
    }
  }

  private def f(b: Int) = { b.toByte }

  private def netAddr(bs: ByteStringBuilder, services: Long, address: NetworkAddress): Unit= {
    bs ++= util.littleEndian8(services)
    val len = address.ip.length
    if (len == 4) {
      bs ++= Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff) map f// 12 ipv4 prefix
    }
    if (isAddressable(address.ip)) {
      bs ++= address.ip
      bs ++= util.network2(address.port)
    } else {
      bs ++= Array.fill(len + 2)(0.toByte)
    }
  }
*/
}
