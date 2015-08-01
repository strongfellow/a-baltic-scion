/**
import collection.mutable.Stack
import org.scalatest._
import a.baltic.scion.util
import a.baltic.scion.message.verackMessage
import a.baltic.scion.message.versionMessage
import a.baltic.scion.domain.NetworkAddress

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
      val b = Seq((i * 16 + j).toByte)
      val h = chars.substring(i, i + 1) + chars.substring(j, j + 1)
      util.hex(b) should be (h)
      util.unHex(h) should be (b)
    }
  }

  "verack" should "be correct" in {
    val msg = verackMessage
    util.hex(msg) should be("f9beb4d976657261636b000000000000000000005df6e0e2")
  }
  
  "versionMessage" should "be correct" in {
    val msg = versionMessage(
        60002L, // protocol version
        1L, // services
        1355854353L, // timestamp 
        NetworkAddress(Array(127, 0, 0, 1), 8333), // remote address
        NetworkAddress(Array(127, 0, 0, 1), 12345), // local address
        7284544412836900411L, // little endian nonce 
        "/Satoshi:0.7.2/", // UserAgent
        212672L // startHeight
        )
    util.hex(msg) should be ("f9beb4d976657273696f6e0000000000640000003b648d5a62ea0000010000000000000011b2d05000000000010000000000000000000000000000000000ffff000000000000010000000000000000000000000000000000ffff0000000000003b2eb35d8ce617650f2f5361746f7368693a302e372e322fc03e0300")
  }

}
*/
