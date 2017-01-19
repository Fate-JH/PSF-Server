// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.newcodecs.newcodecs
import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._
import shapeless.{::, HNil}

/**
  * The player is interacting with a zipline.
  * @param player_guid the player
  * @param origin_side whether this corresponds with the "entry" or the "exit" of the zipline, as per the direction of the light pulse visuals
  * @param action na; usually 0?
  * @param id na; a number that is consistent to a zipline point but is not a building GUID
  * @param unk3 na; changes as the user moves
  * @param unk4 na; changes as the user moves
  * @param unk5 na; changes as the user moves
  */
final case class ZipLineMessage(player_guid : PlanetSideGUID,
                                origin_side : Boolean,
                                action : Int,
                                id : Long,
                                unk3 : Long = 0L,
                                unk4 : Long = 0L,
                                unk5 : Long = 0L)
  extends PlanetSideGamePacket {
  type Packet = ZipLineMessage
  def opcode = GamePacketOpcode.ZipLineMessage
  def encode = ZipLineMessage.encode(this)
}

object ZipLineMessage extends Marshallable[ZipLineMessage] {
  type threeLongsPattern = Long :: Long :: Long :: HNil

  /**
    * A `Codec` for when three `Long` values are to be read or written.
    */
  val threeLongValues : Codec[threeLongsPattern] = (
    ("unk3" | uint32L) ::
      ("unk4" | uint32L) ::
      ("unk5" | uint32L)
    ).as[threeLongsPattern]

  /**
    * A `Codec` for when there are no extra `Long` values are present.
    */
  val noLongValues : Codec[threeLongsPattern] = ignore(0).xmap[threeLongsPattern] (
    {
      case _ =>
        0L :: 0L :: 0L :: HNil
    },
    {
      case _ =>
        ()
    }
  )

  implicit val codec : Codec[ZipLineMessage] = (
    ("player_guid" | PlanetSideGUID.codec) >>:~ { player =>
      ("origin_side" | bool) ::
        ("action" | uint2) ::
        ("id" | uint32L) ::
        newcodecs.binary_choice(player.guid > 0, threeLongValues, noLongValues) // !(player.guid == 0)
    }
    ).as[ZipLineMessage]
}
