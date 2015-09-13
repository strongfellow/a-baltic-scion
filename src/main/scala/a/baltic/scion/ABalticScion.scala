
package a.baltic.scion

import akka.actor.Actor
import akka.actor.Props
import java.net.InetSocketAddress
import a.baltic.scion.tcp.Listener
import a.baltic.scion.actors.TcpClient
import a.baltic.scion.actors.BitcoinDeserializer
import a.baltic.scion.actors.BitcoinSerializer
import a.baltic.scion.actors.PeerConnection
import scala.runtime.RichLong

class ABalticScion extends Actor {

  override def preStart(): Unit = {
    val magic = 0xD9B4BEF9L.asInstanceOf[AnyRef]
    println("starting")
    val remote = new InetSocketAddress("localhost", 8333)
    val tcpClient = context.actorOf(
        Props.create(classOf[TcpClient], remote))
    val serializer = context.actorOf(
        Props.create(classOf[BitcoinSerializer], tcpClient, magic))
    val peerConnection = context.actorOf(
        Props.create(classOf[PeerConnection], serializer))
    val deserializer = context.actorOf(
        Props.create(classOf[BitcoinDeserializer], peerConnection, magic))
    tcpClient ! a.baltic.scion.messages.Register(deserializer)
    tcpClient ! a.baltic.scion.messages.Connect

  }

  def receive = {
    case "ABORT RIGHT NOW" => context.stop(self)
  }
}
