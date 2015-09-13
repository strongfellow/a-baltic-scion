package a.baltic.scion.actors

import akka.actor.Actor
import akka.util.ByteString
import a.baltic.scion.domain.payload.MessageParser
import a.baltic.scion.domain.payload.BitcoinMessageEnvelope
import akka.actor.ActorRef
import a.baltic.scion.messages.ParseFailedEvent
import a.baltic.scion.messages.InvalidNetworkEvent
import akka.actor.Props
import akka.event.Logging

object BitcoinDeserializer {
  def props(next: ActorRef, network: Long) =
    Props(new BitcoinDeserializer(next, network))
}

/**
 * @author andrew
 */
class BitcoinDeserializer(next: ActorRef, network: Long) extends Actor {

   val log = Logging(context.system, this)

  def receive = {
    case bytes:ByteString =>
      log.info("{}", bytes.slice(4, 16).filter(b => b != 0).map(x => x.toChar).mkString(""))
      val parsed = MessageParser.parseBitcoinMessageEnvelope(bytes, 0)
      parsed match {
        case Some((x, _)) =>
          if (x.magic == network) {
            next ! x.payload
          } else {
            next ! InvalidNetworkEvent(network, x.magic)
          }
        case None => next ! ParseFailedEvent(bytes)
      }
    case x => next ! x
  }
}
