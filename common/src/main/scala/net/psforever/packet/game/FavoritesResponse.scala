// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * na
  * @param player_guid the player
  * @param unk1 na
  * @param unk2 na
  * @param unk3 na
  */
final case class FavoritesResponse(player_guid : PlanetSideGUID,
                                  unk1 : Int,
                                  unk2 : Int,
                                  unk3 : Int)
  extends PlanetSideGamePacket {
  type Packet = FavoritesResponse
  def opcode = GamePacketOpcode.FavoritesResponse
  def encode = FavoritesResponse.encode(this)
}

object FavoritesResponse extends Marshallable[FavoritesResponse] {
  implicit val codec : Codec[FavoritesResponse] = (
    ("player_guid" | PlanetSideGUID.codec) ::
      ("unk1" | uint2L) ::
      ("unk2" | uint2L) ::
      ("unk3" | uint2L)
    ).as[FavoritesResponse]
}
