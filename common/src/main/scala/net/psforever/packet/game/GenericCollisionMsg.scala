// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._

final case class GenericCollisionMsg(unk1 : Int,
                                     player1 : PlanetSideGUID,
                                     player2 : PlanetSideGUID,
                                     unk4 : Int,
                                     unk5 : Int,
                                     unk6 : Long,
                                     unk7 : Long,
                                     unk8 : Long,
                                     unk9 : Long,
                                     unkA : Long,
                                     unkB : Long,
                                     player1_pos : Vector3,
                                     player2_pos : Vector3,
                                     unkG : Long,
                                     unkH : Long,
                                     unkI : Long)
  extends PlanetSideGamePacket {
  type Packet = GenericCollisionMsg
  def opcode = GamePacketOpcode.GenericCollisionMsg
  def encode = GenericCollisionMsg.encode(this)
}

object GenericCollisionMsg extends Marshallable[GenericCollisionMsg] {
  implicit val codec : Codec[GenericCollisionMsg] = (
    ("unk1" | uint2) ::
      ("player1" | PlanetSideGUID.codec) ::
      ("player2" | PlanetSideGUID.codec) ::
      ("unk4" | uint16L) ::
      ("unk5" | uint16L) ::
      ("unk6" | uint32L) ::
      ("unk7" | uint32L) ::
      ("unk8" | uint32L) ::
      ("unk9" | uint32L) ::
      ("unkA" | uint32L) ::
      ("unkB" | uint32L) ::
      ("player1_pos" | Vector3.codec_pos) ::
      ("player2_pos" | Vector3.codec_pos) ::
      ("unkG" | uint32L) ::
      ("unkH" | uint32L) ::
      ("unkI" | uint32L)
    ).as[GenericCollisionMsg]
}
