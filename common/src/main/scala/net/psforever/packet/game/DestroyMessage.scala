// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._

/**
  * na
  * @param unk1 na
  * @param unk2 na
  * @param unk3 na
  * @param pos na
  */
final case class DestroyMessage(unk1 : Int,
                                unk2 : Int,
                                unk3 : Int,
                                pos : Vector3)
  extends PlanetSideGamePacket {
  type Packet = DestroyMessage
  def opcode = GamePacketOpcode.DestroyMessage
  def encode = DestroyMessage.encode(this)
}

object DestroyMessage extends Marshallable[DestroyMessage] {
  implicit val codec : Codec[DestroyMessage] = (
    ("unk1" | uint16L) ::
      ("unk2" | uint16L) ::
      ("unk3" | uint16L) ::
      ("pos" | Vector3.codec_pos)
    ).as[DestroyMessage]
}
