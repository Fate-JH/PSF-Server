// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.udp

/**
  * A message received from a specific source.
  * @param msg the message received
  * @param from the socket that the message came across
  */
final case class ReceivedPacket(msg : scodec.bits.ByteVector, from : java.net.InetSocketAddress)
