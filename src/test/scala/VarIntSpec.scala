

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import a.baltic.scion.domain.payload.MessageWriter

class VarIntSpec extends FlatSpec with Matchers {
  
  case class TestCase(n:Long, expectedBytes:Seq[Int])
  
  def test(t:TestCase) = {
    val actual = MessageWriter.writeVarInt(t.n)
    val expected = t.expectedBytes map { x => x.toByte }
    actual should be(expected)
  }
  
  for (t <- Seq(
        TestCase(0L, Seq(0)),
        TestCase(1L, Seq(1)),
        TestCase(73L, Seq(73)),
        TestCase(0xfbL, Seq(0xfb)),
        TestCase(0xfcL, Seq(0xfc)),

        TestCase(0xfdL, Seq(0xfd, 0xfd, 0)),
        TestCase(0xfeL, Seq(0xfd, 0xfe, 0)),
        TestCase(0xa1b5L, Seq(0xfd, 0xb5, 0xa1)),
        TestCase(0xfffeL, Seq(0xfd, 0xfe, 0xff)),
        TestCase(0xffffL, Seq(0xfd, 0xff, 0xff)),

        TestCase(0x010000L, Seq(0xfe, 0, 0, 1, 0)),
        TestCase(0x010001L, Seq(0xfe, 1, 0, 1, 0)),
        TestCase(0x0ab0af30L, Seq(0xfe, 0x30, 0xaf, 0xb0, 0x0a)),
        TestCase(0xfffffffeL, Seq(0xfe, 0xfe, 0xff, 0xff, 0xff)),
        TestCase(0xffffffffL, Seq(0xfe, 0xff, 0xff, 0xff, 0xff)),
        
        TestCase(0x100000000L, Seq(0xff, 0, 0, 0, 0, 1, 0, 0, 0)),
        TestCase(0x100000001L, Seq(0xff, 1, 0, 0, 0, 1, 0, 0, 0)),
        TestCase(0xfffffffffffffffeL, Seq(0xff, 0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff)),
        TestCase(0xffffffffffffffffL, Seq(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff))        
    )) {
    "varInt(%016x)".format(t.n) should ("be " + t.expectedBytes.mkString("[", ",", "]")) in {
      test(t)
    }
  }
}

