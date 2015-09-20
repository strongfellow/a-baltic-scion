package a.baltic.scion.actors

import akka.actor.ActorRef
import akka.actor.Actor
import a.baltic.scion.domain.payload.BitcoinMessage
import a.baltic.scion.domain.payload.BitcoinMessageEnvelope
import akka.util.ByteString
import a.baltic.scion.domain.payload.MessageWriter


/**
 * @author andrew
 */
class BitcoinSerializer(next: ActorRef, network: Long) extends Actor {

  def receive = {
    case payload:BitcoinMessage =>
      val envelope = BitcoinMessageEnvelope(network, payload)
      val bytes = ByteString() ++ MessageWriter.write(envelope)
      next ! bytes
    case x => next ! x
  }
}
