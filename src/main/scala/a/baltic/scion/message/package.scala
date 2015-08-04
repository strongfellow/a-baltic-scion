package a.baltic.scion

/**
 * @author andrew
 */
import akka.util.ByteString
import akka.util.ByteString.ByteString1
import akka.util.ByteStringBuilder
import a.baltic.scion.domain.NetworkAddress
import a.baltic.scion.domain.NetworkAddress
import a.baltic.scion.domain.NetworkAddress
package object message {

  case class Message(magic: Long, command: String, payload: Seq[Byte])

  sealed abstract class Payload {}

  case class VersionPayload(
    version: Long, // 4 bytes
    services: Long, // 8 bytes
    timestamp: Long, // 8 bytes
    receiver: NetworkAddress, // 26 bytes
    emitter: NetworkAddress, // 26 bytes
    nonce: Long, // 8 bytes
    userAgent: String, // ? bytes
    startHeight: Long, // 4 bytes
    relay: Boolean // 1 byte
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

  def parseMessage(s: ByteString): Option[Message] = {
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
  
  val MAIN:Array[Byte] = Array(0xF9, 0xBE, 0xB4, 0xD9).map(_.toByte)
  
  def m(s:String):Array[Byte] = s.getBytes ++ new Array[Byte](12 - s.length)
  
  val VERACK = m("verack")

  def verackMessage = message(MAIN, "verack", Seq())

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

  def message(magic:Seq[Byte],
      command:String,
      payload:Seq[Byte]):Seq[Byte] = {
    val result = new ByteStringBuilder()
    result ++= magic
    result ++= command.getBytes
    result ++= new Array[Byte](12 - command.length)
    result ++= util.littleEndian4(payload.length)
    result ++= util.checksum(payload)
    result ++= payload
    result.result()
  }
}
