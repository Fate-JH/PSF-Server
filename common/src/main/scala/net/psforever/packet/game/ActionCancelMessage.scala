// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * na
  * @param unk1 na
  * @param unk2 na
  * @param unk3 na
  */
final case class ActionCancelMessage(unk1 : Int,
                                     unk2 : Int,
                                     unk3 : Int)
  extends PlanetSideGamePacket {
  type Packet = ActionCancelMessage
  def opcode = GamePacketOpcode.ActionCancelMessage
  def encode = ActionCancelMessage.encode(this)
}

object ActionCancelMessage extends Marshallable[ActionCancelMessage] {
  implicit val codec : Codec[ActionCancelMessage] = (
    ("unk1" | uint16L) ::
      ("unk2" | uint16L) ::
      ("unk4L" | uint4L)
    ).as[ActionCancelMessage]
}
