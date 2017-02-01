// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

//paired? 21 6 00000000 0 21 7 00000000 0
/**
  * na
  * Tend to be sent alongside other ActionProgressMessages.
  * @param unk1 na
  * @param unk2 na
  */
final case class ActionProgressMessage(unk1 : Int,
                                       unk2 : Long)
  extends PlanetSideGamePacket {
  type Packet = ActionProgressMessage
  def opcode = GamePacketOpcode.ActionProgressMessage
  def encode = ActionProgressMessage.encode(this)
}

object ActionProgressMessage extends Marshallable[ActionProgressMessage] {
  implicit val codec : Codec[ActionProgressMessage] = (
    ("unk1" | uint4L) ::
      ("unk2" | uint32L)
    ).as[ActionProgressMessage]
}
