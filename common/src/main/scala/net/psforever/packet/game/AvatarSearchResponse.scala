// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * na
  * @param unk1 na
  * @param unk2 na
  */
final case class AvatarSearchResponse(unk1 : Long,
                                      unk2 : List[Long])
  extends PlanetSideGamePacket {
  type Packet = AvatarSearchResponse
  def opcode = GamePacketOpcode.AvatarSearchResponse
  def encode = AvatarSearchResponse.encode(this)
}

object AvatarSearchResponse extends Marshallable[AvatarSearchResponse] {
  implicit val codec : Codec[AvatarSearchResponse] = (
    ("unk1" | uint32L) ::
      ("unk2" | listOfN(uint8L, uint32L))
    ).as[AvatarSearchResponse]
}
