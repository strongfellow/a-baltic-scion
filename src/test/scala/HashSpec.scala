
import collection.mutable.Stack
import org.scalatest._
import a.baltic.scion.util
import a.baltic.scion.domain.payload.VersionMessage
import a.baltic.scion.domain.payload.NetworkAddress
import a.baltic.scion.domain.payload.MessageWriter
import a.baltic.scion.domain.payload.BitcoinMessageEnvelope
import a.baltic.scion.domain.payload.MessageParser
import a.baltic.scion.domain.payload.AddrMessage
import a.baltic.scion.domain.payload.AddrMessage
import a.baltic.scion.domain.payload.VerackMessage

class HashSpec extends FlatSpec with Matchers {

  val chars = "0123456789abcdef"

  "hash of hello" should "be correct" in {
    val hello = "hello".getBytes
    val actual = util.doubleSHA256(hello)
    val expected = util.unHex("9595c9df90075148eb06860365df33584b75bff782a510c6cd4883a419833d50")
    actual should be (expected)
  }

  "hexes" should "be correct" in {
    for (i <- 0 until 16;
         j <- 0 until 16) {
      val b = Vector((i * 16 + j).toByte)
      val h = chars.substring(i, i + 1) + chars.substring(j, j + 1)
      util.hex(b) should be (h)
      util.unHex(h) should be (b)
    }
  }

  "verack" should "be correct" in {
    val msg = BitcoinMessageEnvelope(0xD9B4BEF9L, VerackMessage())
    val bytes = MessageWriter.write(msg)
    util.hex(bytes) should be("f9beb4d976657261636b000000000000000000005df6e0e2")
    MessageParser.parseBitcoinMessageEnvelope(bytes, 0) should be(Some((msg, bytes.length)))
  }

  "versionMessage" should "be correct" in {
    val bs = Array(0,0,0,0,0,0,0,0,0,0,0xff, 0xff, 127, 0, 0, 1).map(_.toByte)
    val localhost = java.net.InetAddress.getByAddress(bs)
    val bs2 = Array(0,0,0,0,0,0,0,0,0,0,0xff, 0xff, 0, 0, 0, 0).map(_.toByte)
    val empty = java.net.InetAddress.getByAddress(bs2)
    val msg = BitcoinMessageEnvelope(0xD9B4BEF9L, VersionMessage(
        60002L, // protocol version
        1L, // services
        1355854353L, // timestamp 
        NetworkAddress(None, 1, localhost, 8333), // remote address
        NetworkAddress(None, 1, localhost, 12345), // local address
        7284544412836900411L, // little endian nonce 
        "/Satoshi:0.7.2/", // UserAgent
        212672L,
        true// startHeight
        ))
    val msg2 = BitcoinMessageEnvelope(0xD9B4BEF9L, VersionMessage(
        60002L, // protocol version
        1L, // services
        1355854353L, // timestamp 
        NetworkAddress(None, 1, empty, 0), // remote address
        NetworkAddress(None, 1, empty, 0), // local address
        7284544412836900411L, // little endian nonce 
        "/Satoshi:0.7.2/", // UserAgent
        212672L,
        true// startHeight
        ))

    val bytes = MessageWriter.write(msg)
    MessageParser.parseBitcoinMessageEnvelope(bytes, 0) should be(Some((msg2, bytes.length)))
    util.hex(bytes) should be ("f9beb4d976657273696f6e0000000000640000003b648d5a62ea0000010000000000000011b2d05000000000010000000000000000000000000000000000ffff000000000000010000000000000000000000000000000000ffff0000000000003b2eb35d8ce617650f2f5361746f7368693a302e372e322fc03e0300")
  }

  "addrMessage" should "be correct" in {
    val bs = Array(0,0,0,0,0,0,0,0,0,0,0xff, 0xff, 10, 0, 0, 1).map(_.toByte)
    val localhost = java.net.InetAddress.getByAddress(bs)
    val msg = BitcoinMessageEnvelope(0xD9B4BEF9L, AddrMessage(
      Vector(NetworkAddress(Some(0x4D1015E2L), 1, localhost, 8333)
    )))
    val bytes = util.unHex("f9beb4d96164647200000000000000001f000000ed52399b01e215104d010000000000000000000000000000000000ffff0a000001208d")
    MessageParser.parseBitcoinMessageEnvelope(bytes, 0) should be(Some(msg, bytes.length))
  }
}
