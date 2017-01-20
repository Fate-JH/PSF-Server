// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._


final case class ItemTransactionResultMessage(terminal_guid : PlanetSideGUID,
                                              Unk1 : Int,
                                              Unk2 : Boolean,
                                              Unk3 : Int)
  extends PlanetSideGamePacket {
  type Packet = ItemTransactionResultMessage
  def opcode = GamePacketOpcode.ItemTransactionResultMessage
  def encode = ItemTransactionResultMessage.encode(this)
}

object ItemTransactionResultMessage extends Marshallable[ItemTransactionResultMessage] {
  implicit val codec : Codec[ItemTransactionResultMessage] = (
    ("terminal_guid" | PlanetSideGUID.codec) ::
      ("Unk1" | uintL(1)) ::
      ("Unk2" | bool) ::
      ("Unk3" | uint8L)
    ).as[ItemTransactionResultMessage]
}
