// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

final case class ChildObjectStateMessage(data : ByteVector)
  extends PlanetSideGamePacket {
  type Packet = ChildObjectStateMessage
  def opcode = GamePacketOpcode.ChildObjectStateMessage
  def encode = ChildObjectStateMessage.encode(this)
}

object ChildObjectStateMessage extends Marshallable[ChildObjectStateMessage] {
  implicit val codec : Codec[ChildObjectStateMessage] = (
    "data" | bytes
    ).as[ChildObjectStateMessage]
}
