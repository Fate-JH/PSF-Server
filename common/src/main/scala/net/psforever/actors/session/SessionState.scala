// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.session

sealed trait SessionState

final case class New() extends SessionState

final case class Handshaking() extends SessionState

final case class Established() extends SessionState

final case class Related() extends SessionState

final case class Closing() extends SessionState

final case class Closed() extends SessionState
