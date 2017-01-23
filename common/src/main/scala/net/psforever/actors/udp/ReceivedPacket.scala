// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.udp

final case class ReceivedPacket(msg : scodec.bits.ByteVector, from : java.net.InetSocketAddress)
