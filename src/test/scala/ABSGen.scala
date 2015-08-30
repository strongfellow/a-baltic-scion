
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop
import org.scalacheck.Gen
import org.scalacheck.Gen.{listOf, listOfN, oneOf, const}

import a.baltic.scion.domain.payload._

object ABSGen {

  val genByteVector = for {
    bs <- Gen.listOf(arbitrary[Byte])
  } yield bs.toVector

  
  val genHash = for {
    h <- listOfN(32, arbitrary[Byte])
  } yield h.toVector

  val genIpv6 = for {
    ip <- listOfN(16, arbitrary[Byte])
  } yield java.net.InetAddress.getByAddress(ip.toArray)

  val genIpv4 = for {
    ip <- listOfN(4, arbitrary[Byte])
  } yield java.net.InetAddress.getByAddress(ip.toArray)

  val gen4Bytes = Gen.choose(0L, 2L * Int.MaxValue)
  val gen8Bytes = arbitrary[Long]
  val genUserAgent = Gen.identifier
  
  val genNetworkAddressNoTimestamp = for {
    services <- arbitrary[Long]
    ip <- oneOf(genIpv6, genIpv4)
    port <- Gen.choose(1, 65535)
  } yield NetworkAddress(None, services, ip, port)
  
  val genNetworkAddressYesTimestamp = for {
    timestamp <- arbitrary[Long]
    services <- arbitrary[Long]
    ip <- oneOf(genIpv6, genIpv4)
    port <- Gen.choose(1, 65535)
  } yield NetworkAddress(Some(timestamp), services, ip, port)
  
  val genInventoryVector = for {
    invType <- gen4Bytes
    hash <- genHash
  } yield Inventory(invType, hash)
  
  val genTxin = for {
    hash <- genHash
    index <- gen4Bytes
    script <- genByteVector
    sequence <- gen4Bytes
  } yield TxIn(hash, index, script, sequence)
  
  val genTxout = for {
    value <- arbitrary[Long]
    script <- genByteVector
  } yield TxOut(value, script)
  
  val genAscii = for {
    xs <- listOf(Gen.choose[Byte](0, Byte.MaxValue))
  } yield xs.mkString("")
  
  final val genBlockHeader = for {
    version <- gen4Bytes
    previousBlock <- genHash
    merkle <- genHash
    timestamp <- gen4Bytes
    bits <- gen4Bytes
    nonce <- gen4Bytes
  } yield BlockHeader(version, previousBlock, merkle, timestamp, bits, nonce)
  


  
  val genMagic = Gen.oneOf(0xD9B4BEF9L, 0xDAB5BFFAL, 0x0709110BL, 0xFEB4BEF9L)
  val genVersionMessage: Gen[BitcoinMessage] = for {
    version <- gen4Bytes
    services <- gen4Bytes
    timestamp <- gen8Bytes
    to <- genNetworkAddressNoTimestamp
    from <- genNetworkAddressNoTimestamp
    nonce <- arbitrary[Long]
    userAgent <- genUserAgent
    startHeight <- gen4Bytes
    relay <- arbitrary[Boolean]
  } yield VersionMessage(
    version, services, timestamp, to, from, nonce, userAgent, startHeight, (version < 70000 || relay)
  )
  
  val genVerackMessage = const(VerackMessage())
  val genAddrMessage = for {
    invs <- listOf(genNetworkAddressYesTimestamp)
  } yield AddrMessage(invs.toVector)

  val genInvMessage = for {
    invs <- listOf(genInventoryVector)
  } yield InvMessage(invs.toVector)

  val genGetDataMessage = for {
    invs <- listOf(genInventoryVector)
  } yield GetDataMessage(invs.toVector)
  
  val genNotFoundMessage = for {
    invs <- listOf(genInventoryVector)
  } yield NotFoundMessage(invs.toVector)
  
  val genGetBlocksMessage = for {
    version <- gen4Bytes
    hashes <- listOf(genHash)
    hashStop <- genHash
  } yield GetBlocksMessage(version, hashes.toVector, hashStop)
  
  val genGetHeadersMessage = for {
    version <- gen4Bytes
    hashes <- listOf(genHash)
    hashStop <- genHash
  } yield GetHeadersMessage(version, hashes.toVector, hashStop)
  
  val genTxMessage = for {
    version <- gen4Bytes
    txins <- listOf(genTxin)
    txouts <- listOf(genTxout)
    lockTime <- gen4Bytes
  } yield TxMessage(version, txins.toVector, txouts.toVector, lockTime)

  val genBlockMessage = for {
    header <- genBlockHeader
    transactions <- listOf(genTxMessage)
  } yield BlockMessage(header, transactions.toVector)

  val genHeadersMessage = for {
    headers <- listOf(genBlockMessage)
  } yield HeadersMessage(headers.toVector)

  val genGetAddrMessage = const(GetAddrMessage())
  val genMemPoolMessage = const(MemPoolMessage())

  val genCheckOrderMessage = const(CheckOrderMessage())
  val genSubmitOrderMessage = const(SubmitOrderMessage())
  val genReplyMessage = const(ReplyMessage())

  val genPingMessage = for {
    nonce <- arbitrary[Long]
  } yield PingMessage(nonce)

  val genPongMessage = for {
    nonce <- arbitrary[Long]
  } yield PongMessage(nonce)

  val genRejectMessage = for {
    message <- genAscii
    ccode <- arbitrary[Byte]
    reason <- genAscii
    data <- listOf(arbitrary[Byte])
  } yield RejectMessage(message, ccode, reason, data.toVector)

  val genFilterLoadMessage = for {
    filter <- listOf(arbitrary[Byte])
    nHash <- gen4Bytes
    nTweak <- gen4Bytes
    nFlags <- arbitrary[Byte]
  } yield FilterLoadMessage(filter.toVector, nHash, nTweak, nFlags)

  val genFilterAddMessage = for {
    data <- ABSGen.genByteVector
  } yield FilterAddMessage(data)

  val genFilterClearMessage = const(FilterClearMessage())

  val genMerkleBlockMessage = for {
    header <- genBlockHeader
    numTx <- arbitrary[Long]
    hashes <- listOf(genHash)
    flags <- ABSGen.genByteVector
  } yield MerkleBlockMessage(header, numTx, hashes.toVector, flags)

  val genAlertMessage = for {
    payload <- ABSGen.genByteVector
    sig <- ABSGen.genByteVector
  } yield AlertMessage(payload, sig)


  
  val genMessage: Gen[BitcoinMessage] = Gen.oneOf(
    genBlockMessage,
    genBlockMessage

      /**
    genVersionMessage,
    genVerackMessage,
    genAddrMessage,
    genInvMessage,
    genGetDataMessage,
    genNotFoundMessage,
    genGetBlocksMessage,
    genGetHeadersMessage,
    genTxMessage,
    genHeadersMessage,
    genGetAddrMessage,
    genMemPoolMessage,
/**
    genCheckOrderMessage,
    genSubmitOrderMessage,
    genReplyMessage,
 * 
 */
    genPingMessage,
    genPongMessage,
    genRejectMessage,
    genFilterLoadMessage,
    genFilterAddMessage,
    genFilterClearMessage,
    genMerkleBlockMessage,
    genAlertMessage
    * 
    */
  )

  val genBitcoinMessageEnvelope2 = const(
BitcoinMessageEnvelope(118034699,
    BlockMessage(
        BlockHeader(114766439,
      Vector(-69, 127, 123, 127, 127, 43, 1, 127, -1, 1, 93, 0, 0, -128, -1, 1, -128, 127, -128, -68, 80, -1, 1, -124, -128, 127, 0, 1, 127, -1, -33, 127),
      Vector(50, -1, 0, -128, -1, 17, -128, 77, -128, -119, 8, 0, 34, 49, 100, 1, 0, -68, -65, -128, -51, -126, -128, 33, -37, 0, 127, 0, -1, 0, 120, 56),
      1341000135,
      3600879112L,
      3164043241L),
  Vector(
      TxMessage(3511727288L,
      Vector(
          TxIn(
              Vector(-84, 1, 127, 75, 69, 51, 93, -48, -88, -117, 127, -1, 127,
   -43, 119, 0, -18, -1, 1, -1, -54, 1, -128, -38, -119, 19, 0, 0, 127, 127
  , -120, 1),743647144,
  Vector(74),1798067653)),
  Vector(TxOut(1,Vector(1))),1125719729))))  )
  val genBitcoinMessageEnvelope = for {
    magic <- genMagic
    message <- genMessage
  } yield BitcoinMessageEnvelope(magic, message)
}