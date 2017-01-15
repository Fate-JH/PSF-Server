// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._

/**
  * na
  */
final case class DismountVehicleMsg(player_guid : PlanetSideGUID,
                                    unk1 : Int,
                                    unk2 : Boolean)
  extends PlanetSideGamePacket {
  type Packet = DismountVehicleMsg
  def opcode = GamePacketOpcode.DismountVehicleMsg
  def encode = DismountVehicleMsg.encode(this)
}

object DismountVehicleMsg extends Marshallable[DismountVehicleMsg] {
  implicit val codec : Codec[DismountVehicleMsg] = (
    ("player_guid" | PlanetSideGUID.codec) ::
      ("unk1" | uint4L) ::
      ("unk2" | bool)
    ).as[DismountVehicleMsg]
}
