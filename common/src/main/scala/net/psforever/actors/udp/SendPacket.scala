// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.udp

/**
  * A message to be dispatched to a specific location.
  * @param msg the message to be sent
  * @param to the socket address to which the message is to be sent
  */
final case class SendPacket(msg : scodec.bits.ByteVector, to : java.net.InetSocketAddress)
