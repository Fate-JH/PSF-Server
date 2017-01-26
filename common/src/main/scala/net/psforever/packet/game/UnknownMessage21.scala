// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.Codec

/**
  * While this packet has a vestigial case in `ParseTheMessages`, the only things it currently accomplishes are:
  * change the window dimensions;
  * and, crash the client.<br>
  * <br>
  * For the sake of completion, a representation of this packet was still constructed.
  * The code for the aforementioned case is found at line@`60601` in the decompiled client, for the curious.
  * There is no `Encode` or `Decode`.
  * Even if the packet were to pass data, the called function is not actually influenced by its contents.
  */
final case class UnknownMessage21()
  extends PlanetSideGamePacket {
  type Packet = UnknownMessage21
  def opcode = GamePacketOpcode.UnknownMessage21
  def encode = UnknownMessage21.encode(this)
}

object UnknownMessage21 extends Marshallable[UnknownMessage21] {
  implicit val codec : Codec[UnknownMessage21] = PacketHelpers.emptyCodec(UnknownMessage21()).as[UnknownMessage21]
}
