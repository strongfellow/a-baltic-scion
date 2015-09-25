

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import a.baltic.scion.domain.payload.MessageParser
import a.baltic.scion.util
import a.baltic.scion.domain.payload.BlockMessage
import a.baltic.scion.domain.payload.BlockHeader
import a.baltic.scion.domain.payload.TxMessage
import a.baltic.scion.domain.payload.TxIn
import a.baltic.scion.domain.payload.TxOut

class GenesisSpec extends FlatSpec with Matchers {

  "parsing genesis block" should "be correct" in {

    val genesisBlock: Vector[Byte] = util.unHex(
      "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a29ab5f49ffff001d1dac2b7c0101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac00000000").toVector
    val genesisHash: Vector[Byte] = util.unHex(
          "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f").toVector.reverse

    val parsed = MessageParser.parseBlockMessage(genesisBlock, 0)
    parsed should be (Some((BlockMessage(
        BlockHeader(1L,
            util.unHex("0000000000000000000000000000000000000000000000000000000000000000").toVector,
            util.unHex("3BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4A").toVector,
            0x495FAB29L,
            0x1D00FFFF,
            0x7C2BAC1DL
        ),
        Vector(TxMessage(
            1L,
            Vector(TxIn(
                util.unHex("0000000000000000000000000000000000000000000000000000000000000000").toVector,
                0xFFFFFFFFL,
                util.unHex("04FFFF001D0104455468652054696D65732030332F4A616E2F32303039204368616E63656C6C6F72206F6E206272696E6B206F66207365636F6E64206261696C6F757420666F722062616E6B73").toVector,
                0xFFFFFFFFL
            )),
            Vector(TxOut(5000000000L, util.unHex("4104678AFDB0FE5548271967F1A67130B7105CD6A828E03909A67962E0EA1F61DEB649F6BC3F4CEF38C4F35504E51EC112DE5C384DF7BA0B8D578A4C702B6BF11D5FAC").toVector)),
            0L
        )
        )
    ), genesisBlock.length)))

    val actualHash = util.headerHash(parsed.get._1)
    actualHash should be(genesisHash)
  }

}