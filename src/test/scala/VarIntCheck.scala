
import org.scalacheck.Properties
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop
import org.scalacheck.Gen
import org.scalacheck.Gen.{listOf, listOfN, oneOf, const}
import a.baltic.scion.domain.payload.MessageParser.parseVarInt
import a.baltic.scion.domain.payload.MessageWriter.writeVarInt
import a.baltic.scion.domain.payload._

object VarIntCheck extends Properties("VarInt") {
  import org.scalacheck.Prop.forAll

  val oneBytes = Gen.choose(0L, 0xfcL)
  val threeBytes = Gen.choose(0xfdL, 0xffffL)
  val fiveBytes = Gen.choose(0x10000L, 0xffffffffL)
  val nineBytes = Arbitrary.arbitrary[Long] suchThat { n => (n > 0xffffffffL || n < 0) }

  property("oneBytes") = Prop.forAll(oneBytes) { 
    n => (writeVarInt(n) == Seq(n.toByte))
  }

  property("threeBytes") = Prop.forAll(threeBytes) {
    n => (writeVarInt(n) == (Seq(0xfd, n, n >> 8) map { x => (x & 0xff).toByte }))
  }
  
  property("fiveBytes") = Prop.forAll(fiveBytes) {
    n => (writeVarInt(n) == (Seq(0xfe, n, n >> 8, n >> 16, n >> 24) map { x => (x & 0xff).toByte }))
  }

  property("nineBytes") = Prop.forAll(nineBytes) {
    n => (writeVarInt(n) == (Seq(0xff, n, n >> 8, n >> 16, n >> 24, n >> 32, n >> 40, n >> 48, n >> 56) map { x => (x & 0xff).toByte }))
  }

  property("invertible") = Prop.forAll { (n: Long) => 
    val bs = writeVarInt(n)
    parseVarInt(bs, 0) == Some(n, bs.length)
  }
  
  property("littleEndian4") = Prop.forAll(Gen.choose(0L, 2L * Int.MaxValue)) { x =>
    val bytes = MessageWriter.littleEndian4(x)
    MessageParser.parseLittleEndian(bytes, 0, 4) == Some(x, 4)
  }

  property("littleEndian8") = Prop.forAll(arbitrary[Long]) { x =>
    val bytes = MessageWriter.littleEndian8(x)
    MessageParser.parseLittleEndian(bytes, 0, 8) == Some(x, 8)
  }
  
  property("network addresss") = Prop.forAll(ABSGen.genNetworkAddressNoTimestamp) { n =>
    val bytes = MessageWriter.writeNetworkAddress(n, false)
    val parsed = MessageParser.parseNetworkAddress(bytes, 0, false)
    val addr = parsed.get._1
    val nBytes = parsed.get._2
    nBytes == 26 && networksEquivalent(n, addr)
  }
  
  def networksEquivalent(a:NetworkAddress, b:NetworkAddress) = {
    if (a.ip.isSiteLocalAddress() || a.ip.isLoopbackAddress()) {
        (a.timestamp == b.timestamp
          && a.services == b.services
          && b.ip == java.net.InetAddress.getByAddress(
              Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff, 0, 0, 0, 0).map { _.toByte})
          && b.port == 0)
    } else {
      a == b
    }
  }
  
  property("ser-de-ser") = Prop.forAll(ABSGen.genBitcoinMessageEnvelope2) { m =>
    val expectedPayload = m.payload
    val bytes = MessageWriter.write(m)
    val parsedMessage = MessageParser.parseBitcoinMessageEnvelope(bytes, 0)
    val parsed = parsedMessage.get._1
    val len = parsedMessage.get._2
    (expectedPayload, parsed.payload) match {
      case (AddrMessage(as), AddrMessage(bs)) => {
        (as.length == bs.length) && as.zip(bs).forall { case (a, b) => networksEquivalent(a, b) }
      }
      case (VersionMessage(
          version,
          services,
          timestamp,
          to,
          from,
          nonce,
          userAgent,
          startHeight,
          relay
      ), VersionMessage(
          version2,
          services2,
          timestamp2,
          to2,
          from2,
          nonce2,
          userAgent2,
          startHeight2,
          relay2
      )) => {
        (version == version2
            && services == services2
            && timestamp == timestamp2
            && networksEquivalent(to, to2)
            && networksEquivalent(from, from2)
            && nonce == nonce2
            && userAgent == userAgent2
            && startHeight == startHeight2
            && relay == relay2)
      }
      case (a, b) => {
        val result = (a == b)
        result
      }
      case _ => {
        false
      }
    }
  }
}
