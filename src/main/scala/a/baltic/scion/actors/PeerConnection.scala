package a.baltic.scion.actors

import akka.actor.Actor
import akka.event.Logging
import a.baltic.scion.domain.payload.BitcoinMessage


/**
 * @author andrew
 */
class PeerConnection extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case _ => ()
  }
}
