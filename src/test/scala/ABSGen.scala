
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
    sequence <- arbitrary[Long]
  } yield TxIn(hash, index, script, sequence)
  
  val genTxout = for {
    value <- arbitrary[Long]
    script <- genByteVector
  } yield TxOut(value, script)
  
  val genAscii = for {
    xs <- listOf(Gen.choose[Byte](0, Byte.MaxValue))
  } yield xs.mkString("")
  
  final val genBlockHeader = for {
    version <- arbitrary[Int]
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
    version <- arbitrary[Long]
    hashes <- listOf(genHash)
    hashStop <- genHash
  } yield GetBlocksMessage(version, hashes.toVector, hashStop)
  
  val genGetHeadersMessage = for {
    version <- arbitrary[Long]
    hashes <- listOf(genHash)
    hashStop <- genHash
  } yield GetHeadersMessage(version, hashes.toVector, hashStop)
  
  val genTxMessage = for {
    version <- arbitrary[Long]
    txins <- listOf(genTxin)
    txouts <- listOf(genTxout)
    lockTime <- arbitrary[Long]
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
    nHash <- arbitrary[Long]
    nTweak <- arbitrary[Long]
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
    genVersionMessage,
    genVerackMessage,
    genAddrMessage,
    genInvMessage,
    genGetDataMessage,
    genNotFoundMessage,
    genGetBlocksMessage,
    genGetHeadersMessage,
    genTxMessage,
    genBlockMessage,
    genHeadersMessage,
    genGetAddrMessage,
    genMemPoolMessage,
    genCheckOrderMessage,
    genSubmitOrderMessage,
    genReplyMessage,
    genPingMessage,
    genPongMessage,
    genRejectMessage,
    genFilterLoadMessage,
    genFilterAddMessage,
    genFilterClearMessage,
    genMerkleBlockMessage,
    genAlertMessage
  )

  val genBitcoinMessageEnvelope = for {
    magic <- genMagic
    message <- genMessage
  } yield BitcoinMessageEnvelope(magic, message)
}