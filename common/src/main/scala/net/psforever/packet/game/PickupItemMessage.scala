// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * Nothing about this packet is defined.
  * @param unk1 na
  * @param unk2 na
  * @param unk3 na
  * @param unk4 na
  */
final case class PickupItemMessage(unk1 : Int,
                                   unk2 : Int,
                                   unk3 : Int,
                                   unk4 : Int)
  extends PlanetSideGamePacket {
  type Packet = PickupItemMessage
  def opcode = GamePacketOpcode.PickupItemMessage
  def encode = PickupItemMessage.encode(this)
}

object PickupItemMessage extends Marshallable[PickupItemMessage] {
  implicit val codec : Codec[PickupItemMessage] = (
    ("unk1" | uint16L) ::
      ("unk2" | uint16L) ::
      ("unk3" | uint8L) ::
      ("unk4" | uint16L)
    ).as[PickupItemMessage]
}

