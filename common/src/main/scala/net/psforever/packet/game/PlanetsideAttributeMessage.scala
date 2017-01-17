// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

final case class PlanetsideAttributeMessage(unk1 : PlanetSideGUID,
                                            unk2 : Int,
                                            unk3 : Long)
  extends PlanetSideGamePacket {
  type Packet = PlanetsideAttributeMessage
  def opcode = GamePacketOpcode.PlanetsideAttributeMessage
  def encode = PlanetsideAttributeMessage.encode(this)
}

object PlanetsideAttributeMessage extends Marshallable[PlanetsideAttributeMessage] {
  implicit val codec : Codec[PlanetsideAttributeMessage] = (
    ("unk1" | PlanetSideGUID.codec) ::
      ("unk2" | uint8L) ::
      ("unk3" | uint32L)
    ).as[PlanetsideAttributeMessage]
}
