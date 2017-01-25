// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.session

/**
  * Common ancestor for messages used by the various `Session`s.
  */
sealed trait SessionState

/**
  * Announce the start of a session.
  */
final case class New() extends SessionState

/**
  * Announce an attempt at communication between `Sessions`.
  */
final case class Handshaking() extends SessionState

/**
  * Announce that different `Sessions` are now communicating.
  */
final case class Established() extends SessionState

/**
  * This is never used and lacks context.
  */
final case class Related() extends SessionState

/**
  * Announce that one `Session` wants to terminate.
  */
final case class Closing() extends SessionState

/**
  * Announce the termination of a `Session`.
  */
final case class Closed() extends SessionState
