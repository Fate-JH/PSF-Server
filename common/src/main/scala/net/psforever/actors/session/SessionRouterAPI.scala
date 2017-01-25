// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.session

import scodec.bits.ByteVector

/**
  * Common ancestor for messages exclusive to the `SessionRouter`.
  */
sealed trait SessionRouterAPI

/**
  * A packet that is headed downstream.
  * @param data the message being carried by the packet
  */
final case class RawPacket(data : ByteVector) extends SessionRouterAPI

/**
  * A packet that is headed upstream.
  * @param data the message being carried by the packet
  */
final case class ResponsePacket(data : ByteVector) extends SessionRouterAPI

/**
  * Clean up children `Sessions`, looking for ones that are `Closed`.
  */
final case class SessionReaper() extends SessionRouterAPI

/**
  * A child `Session` is self-terminating.
  * @param id the child's `Session` id
  * @param reason the resolution
  */
final case class DropSession(id : Long, reason : String) extends SessionRouterAPI
