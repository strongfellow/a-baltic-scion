package a.baltic.scion.domain

import a.baltic.scion.util.checksum

/**
 * @author andrew
 */
package object payload {

  case class BitcoinMessageEnvelope(
    magic: Long,
    payload: BitcoinMessage
  )

  abstract class BitcoinMessage {}
  
  case class NetworkAddress(
    timestamp: Option[Long], // maybe 4
    services: Long, // 8
    ip: java.net.InetAddress, // 16
    port: Int // 2
  )
  
  case class VersionMessage(
    version: Long, // 4 bytes
    services: Long, // 8 bytes
    timestamp: Long, // 8 bytes
    to: NetworkAddress, // 26
    from: NetworkAddress, // 26
    nonce: Long, // 8
    userAgent: String, // ?
    startHeight: Long, // 4
    relay: Boolean // 1
  ) extends BitcoinMessage

}
