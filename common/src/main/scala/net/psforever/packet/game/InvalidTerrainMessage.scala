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
  * @param unk4 na
  * @param unk5 na
  * @param unk6 na
  */
final case class InvalidTerrainMessage(unk1 : Int,
                                       unk2 : Int,
                                       unk3 : Boolean,
                                       unk4 : Long,
                                       unk5 : Long,
                                       unk6 : Long)
  extends PlanetSideGamePacket {
  type Packet = InvalidTerrainMessage
  def opcode = GamePacketOpcode.InvalidTerrainMessage
  def encode = InvalidTerrainMessage.encode(this)
}

object InvalidTerrainMessage extends Marshallable[InvalidTerrainMessage] {
  implicit val codec : Codec[InvalidTerrainMessage] = (
    ("unk1" | uint16L) ::
      ("unk2" | uint16L) ::
      ("unk3" | bool) ::
      ("unk4" | uint32L) ::
      ("unk5" | uint32L) ::
      ("unk6" | uint32L)
    ).as[InvalidTerrainMessage]
}
