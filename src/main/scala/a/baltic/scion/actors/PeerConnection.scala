package a.baltic.scion.actors

import akka.actor.Actor
import akka.event.Logging
import a.baltic.scion.domain.payload.BitcoinMessage
import akka.actor.FSM

sealed trait State

case object Unconnected extends State
case object Connected extends State
case object ReadInitialized extends State
case object WriteInitialized extends State
case object ReadWriteInitialized extends State

sealed trait Data
case object Uninitialized extends Data

/**
 * @author andrew
 */

class PeerConnection extends FSM[State, Data] {

  startWith(Unconnected, Uninitialized)

  when(Connected) {
    case Event(PeerMessage(_), data) =>
      goto(Connected) using data
  }
}
