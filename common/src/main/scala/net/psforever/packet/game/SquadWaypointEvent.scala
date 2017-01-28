// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.newcodecs.newcodecs
import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._
import shapeless.{::, HNil}

/**
  * na
  * @param unk1 na
  * @param pos na
  * @param unk2 na
  */
final case class WaypointEvent(unk1 : Int,
                               pos : Vector3,
                               unk2 : Int)

/**
  * na
  * @param unk1 na
  * @param unk2 na
  * @param unk3 na
  * @param unk4 na
  * @param unk5 na
  * @param unk6 na
  */
final case class SquadWaypointEvent(unk1 : Int,
                                    unk2 : Int,
                                    unk3 : Long,
                                    unk4 : Int,
                                    unk5 : Option[Long],
                                    unk6 : Option[WaypointEvent])
  extends PlanetSideGamePacket {
  type Packet = SquadWaypointEvent
  def opcode = GamePacketOpcode.SquadWaypointEvent
  def encode = SquadWaypointEvent.encode(this)
}

object SquadWaypointEvent extends Marshallable[SquadWaypointEvent] {
  private type waypointPattern = Option[Long] :: Option[WaypointEvent] :: HNil

  def apply(unk2 : Int, unk3 : Long, unk4 : Int, unk5 : Long) : SquadWaypointEvent =
    new SquadWaypointEvent(1, unk2, unk3, unk4, Some(unk5), None)

  def apply(unk2 : Int, unk3 : Long, unk4 : Int, unkA : Int, x : Float, y : Float, unkB : Int) : SquadWaypointEvent =
    new SquadWaypointEvent(0, unk2, unk3, unk4, None, Some(WaypointEvent(unkA, Vector3(x, y, 0f), unkB)))

  private val longCodec : Codec[waypointPattern] = ("unk5" | uint32L).xmap[waypointPattern] (
    {
      case a =>
        Some(a) :: None :: HNil
    },
    {
      case Some(a) :: _ :: HNil =>
        a
    }
  )

  private val waypointCodec : Codec[waypointPattern] = (
    ("unk1" | uint16L) ::
      ("pos" | Vector3.codec_pos) ::
      ("unk2" | uint(3))
    ).xmap[waypointPattern] (
    {
      case unk1 :: pos :: unk2 :: HNil =>
        None :: Some(WaypointEvent(unk1, pos, unk2)) :: HNil
    },
    {
      case _ :: Some(WaypointEvent(unk1, pos, unk2)) :: HNil =>
        unk1 :: pos :: unk2 ::HNil
    }
  )

  private val noCodec : Codec[waypointPattern] = ignore(0).xmap[waypointPattern] (
    {
      case _ =>
        None :: None :: HNil
    },
    {
      case None :: None :: HNil =>
        ()
    }
  )

  implicit val codec : Codec[SquadWaypointEvent] = (
    ("unk1" | uint2L) >>:~ { value =>
      ("unk2" | uint16L) ::
        ("unk3" | uint32L) ::
        ("unk4" | uint8L) ::
        newcodecs.binary_choice(value > 0,
          newcodecs.binary_choice(value == 1, longCodec, noCodec),
          waypointCodec
        )
    }
    ).as[SquadWaypointEvent]
}
