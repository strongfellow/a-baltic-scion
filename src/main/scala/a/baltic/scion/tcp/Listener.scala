package a.baltic.scion.tcp

import akka.util.ByteString
import akka.actor.Actor
import akka.io.Tcp.Connected
import a.baltic.scion.domain.payload.VersionMessage
import a.baltic.scion.domain.payload.BitcoinMessageEnvelope
import a.baltic.scion.domain.payload.NetworkAddress
import a.baltic.scion.domain.payload.MessageWriter
import a.baltic.scion.domain.payload.MessageParser
import a.baltic.scion.domain.payload.BitcoinMessageEnvelope
import a.baltic.scion.domain.payload.BitcoinMessageEnvelope
import a.baltic.scion.domain.payload.BitcoinMessageEnvelope

/**
 * @author andrew
 */
class Listener extends Actor {

  def receive = {
    case Connected(remote, local) => {
      val remotePort = remote.getPort()
      val localPort = local.getPort()
      println("yeah baby we are connected: " + remote + ", "+ local)
      val msg = BitcoinMessageEnvelope(0xD9B4BEF9L, VersionMessage(
        60002L, // protocol version
        1L, // services
        1355854353L, // timestamp 
        NetworkAddress(None, 1, remote.getAddress(), remotePort), // remote address
        NetworkAddress(None, 1, local.getAddress(), localPort), // local address
        7284544412836900411L, // little endian nonce 
        "/Satoshi:0.7.2/", // UserAgent
        212672L,
        true// startHeight
        ))
      val message = ByteString() ++ MessageWriter.write(msg)
      sender() ! message
    }

    case bytes:ByteString => {
      val message = MessageParser.parseBitcoinMessageEnvelope(bytes, 0)
      println(message)
      message match {
        case Some((BitcoinMessageEnvelope(_, VersionMessage(_, _, _, _, _, _, _, _, _)), _)) =>
          println("version received")
        case Some((BitcoinMessageEnvelope(_, _), _)) =>
          println("something else received")
        case None =>
          println("None received")
      }
    }
  }
}
