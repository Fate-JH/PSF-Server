// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._

/**
  * na
  * @param parent_guid the object previously containing the child object
  * @param child_guid the child object
  * @param pos na
  * @param unk1 na
  * @param unk2 na
  * @param unk3 na
  */
final case class ObjectDetachMessage(parent_guid : PlanetSideGUID,
                                     child_guid : PlanetSideGUID,
                                     pos : Vector3,
                                     unk1 : Int,
                                     unk2 : Int,
                                     unk3 : Int)
  extends PlanetSideGamePacket {
  type Packet = ObjectDetachMessage
  def opcode = GamePacketOpcode.ObjectDetachMessage
  def encode = ObjectDetachMessage.encode(this)
}

object ObjectDetachMessage extends Marshallable[ObjectDetachMessage] {
  implicit val codec : Codec[ObjectDetachMessage] = (
    ("parent_guid" | PlanetSideGUID.codec) ::
      ("child_guid" | PlanetSideGUID.codec) ::
      ("pos" | Vector3.codec_pos) ::
      ("unk1" | uint8L) ::
      ("unk2" | uint8L) ::
      ("unk3" | uint8L)
    ).as[ObjectDetachMessage]
}
