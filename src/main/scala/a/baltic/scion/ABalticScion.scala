
package a.baltic.scion

import akka.actor.Actor
import akka.actor.Props
import java.net.InetSocketAddress
import a.baltic.scion.tcp.Listener
import a.baltic.scion.actors.TcpClient

class ABalticScion extends Actor {

  override def preStart(): Unit = {
    println("starting")
    val remote = new InetSocketAddress("localhost", 8333)
    val listener = context.actorOf(Props[Listener], "listener")
    val tcpClient = context.actorOf(
        Props.create(classOf[TcpClient], remote))
    tcpClient ! a.baltic.scion.messages.Register(listener)
    tcpClient ! a.baltic.scion.messages.Connect
  }

  def receive = {
    case "ABORT RIGHT NOW" => context.stop(self)
  }
}
