// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import shapeless.{::, HNil}

/**
  * na
  * @param unk1 na
  * @param unk2 na
  * @param unk3 na
  */
final case class SetChatFilterMessage(unk1 : Int,
                                      unk2 : Boolean,
                                      unk3 : List[Boolean])
  extends PlanetSideGamePacket {
  type Packet = SetChatFilterMessage
  def opcode = GamePacketOpcode.SetChatFilterMessage
  def encode = SetChatFilterMessage.encode(this)
}

object SetChatFilterMessage extends Marshallable[SetChatFilterMessage] {
  implicit val codec : Codec[SetChatFilterMessage] = (
    ("unk1" | uintL(7)) ::
      ("unk2" | bool) ::
      ("unk3" | PacketHelpers.listOfNSized(9, bool))
    ).exmap[SetChatFilterMessage] (
    {
      case a :: b :: c :: HNil =>
        Attempt.successful(SetChatFilterMessage(a, b, c))
    },
    {
      case SetChatFilterMessage(a, b, c) =>
        if(c.size != 9) {
          Attempt.failure(Err("list size is not 9"))
        }
        else {
          Attempt.successful(a :: b :: c :: HNil)
        }
    }
  )
}
