
package a.baltic.scion

import akka.actor.Actor
import akka.actor.Props
import a.baltic.scion.tcp.Client
import java.net.InetSocketAddress
import a.baltic.scion.tcp.Listener

class ABalticScion extends Actor {

  override def preStart(): Unit = {
    val listener = context.actorOf(Props[Listener], "listener")
    val remote = new InetSocketAddress("localhost", 8333)
    val client = context.actorOf(Props(new Client(remote, listener)))
  }

  def receive = {
    case "ABORT RIGHT NOW" => context.stop(self)
  }
}
