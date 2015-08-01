
import org.scalacheck.Properties
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Gen

import a.baltic.scion.util.varInt

object VarIntCheck extends Properties("VarInt") {
  import org.scalacheck.Prop.forAll

  val oneBytes = Gen.choose(0L, 0xfcL)
  val threeBytes = Gen.choose(0xfdL, 0xffffL)
  val fiveBytes = Gen.choose(0x10000L, 0xffffffffL)
  val nineBytes = Arbitrary.arbitrary[Long] suchThat { n => (n > 0xffffffffL || n < 0) }

  property("oneBytes") = Prop.forAll(oneBytes) { 
    n => (varInt(n) == Seq(n.toByte))
  }

  property("threeBytes") = Prop.forAll(threeBytes) {
    n => (varInt(n) == (Seq(0xfd, n, n >> 8) map { x => (x & 0xff).toByte }))
  }
  
  property("fiveBytes") = Prop.forAll(fiveBytes) {
    n => (varInt(n) == (Seq(0xfe, n, n >> 8, n >> 16, n >> 24) map { x => (x & 0xff).toByte }))
  }

  property("nineBytes") = Prop.forAll(nineBytes) {
    n => (varInt(n) == (Seq(0xff, n, n >> 8, n >> 16, n >> 24, n >> 32, n >> 40, n >> 48, n >> 56) map { x => (x & 0xff).toByte }))
  }

}
