// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.newcodecs.newcodecs
import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._
import shapeless.{::, HNil}

final case class WaypointRequest(unk : Int,
                                 pos : Vector3)

final case class SquadWaypointRequest(unk1 : Int,
                                      unk2 : Long,
                                      unk3 : Int,
                                      unk4 : Option[Long],
                                      unk5 : Option[WaypointRequest])
  extends PlanetSideGamePacket {
  type Packet = SquadWaypointRequest
  def opcode = GamePacketOpcode.SquadWaypointRequest
  def encode = SquadWaypointRequest.encode(this)
}

object SquadWaypointRequest extends Marshallable[SquadWaypointRequest] {
  private type waypointPattern = Option[Long] :: Option[WaypointRequest] :: HNil

  def apply(unk2 : Long, unk3 : Int, unk4 : Long) : SquadWaypointRequest =
    new SquadWaypointRequest(1, unk2, unk3, Some(unk4), None)

  def apply(unk2 : Long, unk3 : Int, unkA : Int, x : Float, y : Float) : SquadWaypointRequest =
    new SquadWaypointRequest(0, unk2, unk3, None, Some(WaypointRequest(unkA, Vector3(x, y, 0f))))

  private val longCodec : Codec[waypointPattern] = ("unk4" | uint32L).xmap[waypointPattern] (
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
    ("unk" | uint16L) ::
      ("pos" | Vector3.codec_pos)
    ).xmap[waypointPattern] (
    {
      case unk :: pos :: HNil =>
        None :: Some(WaypointRequest(unk, pos)) :: HNil
    },
    {
      case _ :: Some(WaypointRequest(unk, pos)) :: HNil =>
        unk :: pos :: HNil
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

  implicit val codec : Codec[SquadWaypointRequest] = (
    ("unk1" | uint2L) >>:~ { value =>
      ("unk2" | uint32L) ::
        ("unk3" | uint8L) ::
        newcodecs.binary_choice(value > 0,
          newcodecs.binary_choice(value == 1, longCodec, noCodec),
          waypointCodec
        )
    }
    ).as[SquadWaypointRequest]
}
