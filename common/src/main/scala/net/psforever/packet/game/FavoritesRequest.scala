// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * This packet is very unstable.
  * @param player_guid the player
  * @param unk1 na
  * @param unk2 na
  * @param unk3 na
  * @param unk4 na
  */
final case class FavoritesRequest(player_guid : PlanetSideGUID,
                                  unk1 : Int,
                                  unk2 : Int,
                                  unk3 : Int,
                                  unk4 : Option[List[Int]] = None)
  extends PlanetSideGamePacket {
  type Packet = FavoritesRequest
  def opcode = GamePacketOpcode.FavoritesRequest
  def encode = FavoritesRequest.encode(this)
}

object FavoritesRequest extends Marshallable[FavoritesRequest] {
  implicit val codec : Codec[FavoritesRequest] = (
    ("player_guid" | PlanetSideGUID.codec) ::
      ("unk1" | uint2L) ::
      (("unk2" | uint2L) >>:~ { unk2 =>
        ("unk3" | uint4L) ::
          conditional(unk2 == 1, "unk4" | listOfN(PacketHelpers.encodedStringSize, uint16L))
        })
    ).as[FavoritesRequest]
}
