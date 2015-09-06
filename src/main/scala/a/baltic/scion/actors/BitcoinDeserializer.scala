package a.baltic.scion.actors

import akka.actor.Actor
import akka.util.ByteString
import a.baltic.scion.domain.payload.MessageParser
import a.baltic.scion.domain.payload.BitcoinMessageEnvelope
import akka.actor.ActorRef
import a.baltic.scion.messages.ParseFailedEvent

/**
 * @author andrew
 */
class BitcoinDeserializer(next: ActorRef) extends Actor {

  def receive = {
    case bytes:ByteString =>
      val parsed = MessageParser.parseBitcoinMessageEnvelope(bytes, 0)
      parsed match {
        case Some((x, _)) => next ! x
        case None => next ! ParseFailedEvent(bytes)
      }
    case x => next ! x
  }
}
