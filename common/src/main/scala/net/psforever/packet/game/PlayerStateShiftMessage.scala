// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._
import shapeless.{::, HNil}

/**
  * na
  * @param unk na
  * @param pos the position to move the character to in the world environment (in three coordinates)
  * @param viewYawLim an angle with respect to the horizon towards which the avatar is looking (to some respect)
  * @param vel the velocity to apply to to the character at the given position (in three coordinates)
  */
final case class PlayerState(unk : Int,
                       pos : Vector3,
                       viewYawLim : Int,
                       vel : Option[Vector3])

/**
  * Force the client's character to adhere to the influence of specific external stimulus.
  * @param state the state to influence the character with respect to his environment in the current zone
  * @param unk na
  */
final case class PlayerStateShiftMessage(state : Option[PlayerState],
                                         unk : Option[Int])
  extends PlanetSideGamePacket {
  type Packet = TimeOfDayMessage
  def opcode = GamePacketOpcode.PlayerStateShiftMessage
  def encode = PlayerStateShiftMessage.encode(this)
}

object PlayerState extends Marshallable[PlayerState] {
  /**
    * An abbreviated constructor for creating `PlayerState`, assuming velocity is not applied.
    * @param unk na
    * @param pos the position of the character in the world environment (in three coordinates)
    * @param viewYawLim an angle with respect to the horizon towards which the avatar is looking (to some respect)
    * @param vel the velocity to apply to to the character at the given position (in three coordinates)
    * @return a `PlayerState` object
    */
  def apply(unk : Int, pos : Vector3, viewYawLim : Int, vel : Vector3) : PlayerState =
  PlayerState(unk, pos, viewYawLim, Some(vel))

  /**
    * An abbreviated constructor for creating `PlayerState`, removing the optional condition of all parameters.
    * @param unk na
    * @param pos the position of the character in the world environment (in three coordinates)
    * @param viewYawLim an angle with respect to the horizon towards which the avatar is looking (to some respect)
    * @return a `PlayerState` object
    */
  def apply(unk : Int, pos : Vector3, viewYawLim : Int) : PlayerState =
    PlayerState(unk, pos, viewYawLim, None)

  implicit val codec : Codec[PlayerState] = (
    ("unk1" | uintL(3)) ::
      ("pos" | Vector3.codec_pos) ::
      ("unk2" | uint8L) ::
      (bool >>:~ { test =>
        ignore(0) ::
          conditional(test, "pos" | Vector3.codec_vel)
      })
    ).xmap[PlayerState] (
      {
        case a :: b :: c :: false :: _ :: None :: HNil =>
          PlayerState(a, b, c, None)
        case a :: b :: c :: true :: _ :: Some(vel) :: HNil =>
          PlayerState(a, b, c, Some(vel))
      },
      {
        case PlayerState(a, b, c, None) =>
          a :: b :: c :: false :: () :: None :: HNil
        case PlayerState(a, b, c, Some(vel)) =>
          a :: b :: c :: true :: () :: Some(vel) :: HNil
      }
    )
}

object PlayerStateShiftMessage extends Marshallable[PlayerStateShiftMessage] {
  /**
    * An abbreviated constructor for creating `PlayerStateShiftMessage`, removing the optional condition of both parameters.
    * @param state the state to which to influence the character with respect to his environment in the current zone
    * @param unk na
    * @return a `PlayerStateShiftMessage` packet
    */
  def apply(state : PlayerState, unk : Int) : PlayerStateShiftMessage =
    PlayerStateShiftMessage(Some(state), Some(unk))

  /**
    * An abbreviated constructor for creating `PlayerStateShiftMessage`, assuming the parameter `unk` is not defined.
    * @param state the state to which to influence the character with respect to his environment in the current zone
    * @return a `PlayerStateShiftMessage` packet
    */
  def apply(state : PlayerState) : PlayerStateShiftMessage =
    PlayerStateShiftMessage(Some(state), None)

  /**
    * An abbreviated constructor for creating `PlayerStateShiftMessage`, assuming the parameter `state` is not defined.
    * @param unk na
    * @return a `PlayerStateShiftMessage` packet
    */
  def apply(unk : Int) : PlayerStateShiftMessage =
    PlayerStateShiftMessage(None, Some(unk))

  implicit val codec : Codec[PlayerStateShiftMessage] = (
    //first part - optional state parameter
    (bool >>:~ { test1 =>
      ignore(0) ::
        conditional(test1, "pos" | PlayerState.codec)
    }).xmap[ Option[PlayerState] ] (
      {
        case false :: _ :: None :: HNil =>
          None
        case true :: _ :: Some(pos) :: HNil =>
          Some(pos)
      },
      {
        case None =>
          false :: () :: None :: HNil
        case Some(pos) =>
          true :: () :: Some(pos) :: HNil
      }
    ) ::
      //second part - optional unknown parameter
      (bool >>:~ { test2 =>
        ignore(0) ::
          conditional(test2, "unk" | uintL(3))
      }).xmap[ Option[Int] ] (
        {
          case false :: _ :: None :: HNil =>
            None
          case true :: _ :: Some(i) :: HNil =>
            Some(i)
        },
        {
          case None =>
            false :: () :: None :: HNil
          case Some(i) =>
            true :: () :: Some(i) :: HNil
        }
      )
    ).as[PlayerStateShiftMessage]
}
