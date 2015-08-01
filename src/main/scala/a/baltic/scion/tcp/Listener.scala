package a.baltic.scion.tcp

import akka.util.ByteString
import akka.actor.Actor
import akka.io.Tcp.Connected

/**
 * @author andrew
 */
class Listener extends Actor {

  def receive = {
    case Connected(remote, local) => {
      println("yeah baby we are connected: " + remote + ", "+ local)
      val message = ByteString("hello")
      sender() ! message
    }

    case x:ByteString => {
      println(a.baltic.scion.util.hex(x))
    }
  }
}
