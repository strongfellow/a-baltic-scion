package a.baltic.scion.tcp

import akka.util.ByteString
import akka.actor.Actor
import akka.io.Tcp.Connected
import a.baltic.scion.domain.NetworkAddress

/**
 * @author andrew
 */
class Listener extends Actor {

  def receive = {
    case Connected(remote, local) => {
      println("yeah baby we are connected: " + remote + ", "+ local)
      val message = a.baltic.scion.message.versionMessage(
        60002L, // protocol version
        1L, // services
        1355854353L, // timestamp 
        NetworkAddress(Array(127, 0, 0, 1), 8333), // remote address
        NetworkAddress(Array(127, 0, 0, 1), 12345), // local address
        7284544412836900411L, // little endian nonce 
        "/Satoshi:0.7.2/", // UserAgent
        212672L // startHeight
      )
      sender() ! message
    }

    case x:ByteString => {
      val messageWrapper = a.baltic.scion.message.parseMessage(x)
      println(messageWrapper)
//      println("listener received: " + a.baltic.scion.util.hex(x))
    }
  }
}
