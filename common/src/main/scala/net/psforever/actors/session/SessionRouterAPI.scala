// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.session

import scodec.bits.ByteVector

sealed trait SessionRouterAPI

final case class RawPacket(data : ByteVector) extends SessionRouterAPI

final case class ResponsePacket(data : ByteVector) extends SessionRouterAPI

final case class SessionReaper() extends SessionRouterAPI

final case class DropSession(id : Long, reason : String) extends SessionRouterAPI
