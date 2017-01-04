// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.newcodecs.newcodecs
import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._
import shapeless.{::, HNil}

/**
  * na
  * @param i na
  * @param j na
  * @param k na
  */
final case class ExtraDoubles(i : Double,
                              j : Double,
                              k : Double)

/**
  * na
  * @param guid the avatar's guid
  * @param pos the position of the avatar in the world environment (in three coordinates)
  * @param unk1 na; three decimal values bound between -256.0 and 256.0
  * @param facingYaw the angle with respect to the horizon towards which the avatar is looking;
  *                  every `0x10` is 45-degrees counter clockwise;
  *                  it wraps to North every `0x80`
  * @param facingPitch the angle with respect to the sky and the ground towards which the avatar is looking;
//  *                  every '0x01' is about 5.625 degrees;
//  *                  `0x00` to `0x10` are downwards-facing angles, with `0x00` as forwards-facing;
//  *                  nothing from `0x11` to `0x29`;
//  *                  `0x30` to `0x40` are upwards-facing angles, with `0x30` starting at full-up;
//  *                  starting at `0x40` == `0x00` this system repeats
  * @param facingYawUpper the angle of the avatar's upper body with respect to its forward-facing direction;
//  *                     `0x00` to `0x10` are the avatar turning to its left, with `0x00` being forward-facing;
//  *                     nothing from `0x11` to `0x29`;
//  *                     `0x30` to `0x40` are the avatar turning to its right, with `0x40` being forward-facing;
//  *                     starting at `0x40` == `0x00` this system repeats
  * @param unk2 na
  * @param fourBools activate parsing for the following four fields, otherwise they will all be false
  * @param unk3 na
  * @param isCrouching avatar is crouching;
  *                    must remain flagged for crouch to maintain animation;
  *                    turn off to stand up
  * @param isJumping avatar is jumping;
  *                  must remain flagged for jump to maintain animation;
  *                  turn off when landed
  * @param unk4 na
  */
final case class PlayerStateMessage(guid : PlanetSideGUID,
                                    pos : Vector3,
                                    unk1 : Option[ExtraDoubles],
                                    facingYaw : Int,
                                    facingPitch : Int,
                                    facingYawUpper : Int,
                                    unk2 : Int,
                                    fourBools : Boolean,
                                    unk3 : Boolean = false,
                                    isCrouching : Boolean = false,
                                    isJumping : Boolean = false,
                                    unk4 : Boolean = false)
  extends PlanetSideGamePacket {
  type Packet = PlayerStateMessage
  def opcode = GamePacketOpcode.PlayerStateMessage
  def encode = PlayerStateMessage.encode(this)
}

object ExtraDoubles extends Marshallable[ExtraDoubles] {
  implicit val codec : Codec[ExtraDoubles] = (
    ("i" | newcodecs.q_double(-256.0, 256.0, 14)) ::
      ("j" | newcodecs.q_double(-256.0, 256.0, 14)) ::
      ("k" | newcodecs.q_double(-256.0, 256.0, 14))
    ).as[ExtraDoubles]
}

object PlayerStateMessage extends Marshallable[PlayerStateMessage] {
  type fourBoolPattern = Boolean :: Boolean :: Boolean :: Boolean :: HNil
  type statePattern = PlanetSideGUID :: Vector3 :: Boolean :: Option[ExtraDoubles] :: Int :: Int :: Int :: Boolean :: Boolean :: Boolean :: Boolean :: HNil

  val booleanCodec : Codec[fourBoolPattern] = (
    ("unk3" | bool) ::
      ("isCrouching" | bool) ::
      ("isJumping" | bool) ::
      ("unk4" | bool)
    ).as[fourBoolPattern]

  val defaultCodec : Codec[fourBoolPattern] = ignore(0).xmap[fourBoolPattern] (
    {
      case _ =>
        false :: false :: false :: false :: HNil
    },
    {
      case _ =>
        ()
    }
  ).as[fourBoolPattern]

  implicit val codec : Codec[PlayerStateMessage] = (
    ("guid" | PlanetSideGUID.codec) ::
      ("pos" | Vector3.codec_pos) ::
      (bool >>:~ { b1 =>
        conditional(b1, "unk1" | ExtraDoubles.codec) ::
          ("facingYaw" | uint8L) ::
          ("facingPitch" | uint8L) ::
          ("facingYawUpper" | uint8L) ::
          ("unk2" | uintL(10)) ::
            ("fourBools" | bool >>:~ { b2 =>
              ignore(0) ::
                newcodecs.binary_choice(b2, booleanCodec, defaultCodec)
            })
    })
    ).xmap[PlayerStateMessage] (
    {
      case uid :: p :: true :: Some(extra) :: f1 :: f2 :: f3 :: u :: b :: _ :: b1 :: b2 :: b3 :: b4 :: HNil =>
        PlayerStateMessage(uid, p, Some(extra), f1, f2, f3, u, b, b1, b2, b3, b4)
      case uid :: p :: false :: None :: f1 :: f2 :: f3 :: u :: b :: _ :: b1 :: b2 :: b3 :: b4 :: HNil =>
        PlayerStateMessage(uid, p, None, f1, f2, f3, u, b, b1, b2, b3, b4)
    },
    {
      case PlayerStateMessage(uid, p, Some(extra), f1, f2, f3, u, b, b1, b2, b3, b4) =>
        uid :: p :: true :: Some(extra) :: f1 :: f2 :: f3 :: u :: b :: () :: b1 :: b2 :: b3 :: b4 :: HNil
      case PlayerStateMessage(uid, p, None, f1, f2, f3, u, b, b1, b2, b3, b4) =>
        uid :: p :: false :: None :: f1 :: f2 :: f3 :: u :: b :: () :: b1 :: b2 :: b3 :: b4 :: HNil
    }
  )
}

/*
There's some kind of decoding here but I've reached the limit to my understanding about how it works.
Extra2.decider and Extra2.len are simplified placeholders for indescribable conditions.

final case class Extra1(unk1 : Int,
                        unk2 : List[Extra2])

final case class Extra2(unk1 : Int,
                        unk2 : Option[Int])

object Extra1 {
  val decider : Boolean = true
  val len : Int = 0

  implicit val codec : Codec[Extra1] = (
    ("unk1" | uint8L) ::
      ("unk2" | newcodecs.binary_choice(decider,
        PacketHelpers.listOfNSized(len, Extra2.oneValueCodec),
        PacketHelpers.listOfNSized(len, Extra2.twoValueCodec)))
    ).as[Extra1]
}

object Extra2 {
  def apply(a : Int) : Extra2 = {
    Extra2(a, None)
  }

  def apply(a : Int, b : Int) : Extra2 = {
    Extra2(a, Some(b))
  }

  val oneValueCodec : Codec[Extra2] = (
    ignore(0) ::
      ("unk2" | uint8L)
    ).xmap[Extra2] (
    {
      case _ :: a :: HNil =>
        Extra2(a, None)
    },
    {
      case Extra2(a, None) =>
        () :: a :: HNil
    }
  )

  val twoValueCodec : Codec[Extra2] = (
    ("unk1" | uint4L) >>:~ { unk =>
      ignore(0) ::
        conditional(unk == 15, "unk2" | uint8L)
    }
    ).xmap[Extra2] (
    {
      case a :: _ :: b :: HNil =>
        Extra2(a, b)
    },
    {
      case Extra2(a, b) =>
        a :: () :: b :: HNil
    }
  )
}
 */
