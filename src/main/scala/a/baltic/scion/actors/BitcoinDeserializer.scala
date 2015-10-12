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
import scala.annotation.tailrec

object BitcoinDeserializer {
  def props(next: ActorRef, network: Long) =
    Props(new BitcoinDeserializer(next, network))
}

/**
 * @author andrew
 */
class BitcoinDeserializer(next: ActorRef, network: Long) extends Actor {

  var buffer = ByteString()
  val log = Logging(context.system, this)

   @tailrec
   private def consume(start: Int): Unit = {
      val parsed = MessageParser.parseBitcoinMessageEnvelope(buffer, start)
      parsed match {
        case Some((x, nextStart)) =>
          log.info("deserialized {}", x)
          if (x.magic == network) {
            next ! x.payload
          } else {
            next ! InvalidNetworkEvent(network, x.magic)
          }
          consume(nextStart)
        case None =>
          buffer = buffer.slice(start, buffer.length)
     }
   }

  def receive = {
    case bytes:ByteString =>
      buffer = buffer ++ bytes
      consume(0)
    case x => next ! x
  }
}
