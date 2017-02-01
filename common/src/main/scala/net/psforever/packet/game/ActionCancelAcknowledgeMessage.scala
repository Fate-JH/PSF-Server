// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * We've never actually received this packet.
  * @param unk1 na
  * @param unk2 na
  */
final case class ActionCancelAcknowledgeMessage(unk1 : Int,
                                                unk2 : Int)
  extends PlanetSideGamePacket {
  type Packet = ActionCancelAcknowledgeMessage
  def opcode = GamePacketOpcode.ActionCancelAcknowledgeMessage
  def encode = ActionCancelAcknowledgeMessage.encode(this)
}

object ActionCancelAcknowledgeMessage extends Marshallable[ActionCancelAcknowledgeMessage] {
  implicit val codec : Codec[ActionCancelAcknowledgeMessage] = (
    ("unk1" | uint16L) ::
      ("unk2" | uint4L)
    ).as[ActionCancelAcknowledgeMessage]
}
