// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * na
  * @param unk1 na
  * @param unk2 na
  * @param unk3 na
  * @param unk4 na
  * @param unk5 na
  */
final case class Weather1(unk1 : Int,
                          unk2 : Float,
                          unk3 : Float,
                          unk4 : Float,
                          unk5 : Float)

/**
  * na
  * @param unk1 na
  * @param unk2 na
  * @param unk3 na
  * @param unk4 na
  */
final case class Weather2(unk1 : Float,
                          unk2 : Float,
                          unk3 : Int,
                          unk4 : Int)

/**
  * na
  * @param unk1 na
  * @param unk2 na
  */
final case class WeatherMessage(unk1 : List[Weather1],
                                unk2 : List[Weather2])
  extends PlanetSideGamePacket {
  type Packet = WeatherMessage
  def opcode = GamePacketOpcode.WeatherMessage
  def encode = WeatherMessage.encode(this)
}

object WeatherMessage extends Marshallable[WeatherMessage] {
  /**
    * na
    */
  private val weather1Codec : Codec[Weather1] = (
    ("unk1" | uint8L) ::
      ("unk2" | floatL) ::
      ("unk3" | floatL) ::
      ("unk4" | floatL) ::
      ("unk5" | floatL)
    ).as[Weather1]

  /**
    * na
    */
  private val weather2Codec : Codec[Weather2] = (
    ("unk1" | floatL) ::
      ("unk2" | floatL) ::
      ("unk3" | uint8L) ::
      ("unk4" | uint8L)
    ).as[Weather2]

  implicit val codec : Codec[WeatherMessage] = (
    ("unk1" | PacketHelpers.listOfNAligned(uint32L, 0, weather1Codec)) ::
      ("unk2" | PacketHelpers.listOfNAligned(uint32L, 0, weather2Codec))
    ).as[WeatherMessage]
}
