// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.udp

final case class SendPacket(msg : scodec.bits.ByteVector, to : java.net.InetSocketAddress)
