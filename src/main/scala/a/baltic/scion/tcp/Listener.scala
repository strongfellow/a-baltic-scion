package a.baltic.scion.tcp

import akka.util.ByteString
import akka.actor.Actor
import akka.io.Tcp.Connected
import a.baltic.scion.domain.payload.VersionMessage
import a.baltic.scion.domain.payload.BitcoinMessageEnvelope
import a.baltic.scion.domain.payload.NetworkAddress
import a.baltic.scion.domain.payload.MessageParser
import a.baltic.scion.domain.payload.BitcoinMessageEnvelope
import a.baltic.scion.domain.payload.BitcoinMessageEnvelope
import a.baltic.scion.domain.payload.BitcoinMessageEnvelope
import a.baltic.scion.domain.payload.InvMessage
import akka.event.Logging
import a.baltic.scion.domain.payload.MessageWriter

/**
 * @author andrew
 */
class Listener extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case Connected(remote, local) => {
      log.info("yeah baby we are connected from local {} to remote {}", local, remote)
      val msg = BitcoinMessageEnvelope(0xD9B4BEF9L, VersionMessage(
        70002L, // protocol version
        1L, // services
        1355854353L, // timestamp
        NetworkAddress(None, 1, Vector(remote.getAddress().getAddress(): _*), remote.getPort()), // remote address
        NetworkAddress(None, 1, Vector(local.getAddress().getAddress(): _*), local.getPort()), // local address
        7284544412836900411L, // little endian nonce
        "/ABalticScion/", // UserAgent
        0L,
        true
        ))
      val message = ByteString() ++ MessageWriter.write(msg)
      sender() ! message
    }

    case bytes:ByteString => {
      val message = MessageParser.parseBitcoinMessageEnvelope(bytes, 0)
      message match {
        case Some((BitcoinMessageEnvelope(magic, message), _)) => {
          message match {
            case InvMessage(invs) => {
              for {
                i <- invs
              } println("inv: " + a.baltic.scion.util.hex(i.hash.reverse))
            }
            case _ => {

            }
          }
        }
        case None => println("None received")
        case _ => println("now i'm confused")
      }
    }
  }
}
