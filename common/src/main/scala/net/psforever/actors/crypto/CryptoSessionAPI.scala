// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.crypto

/**
  * Common ancestor for messages exclusive to the `CryptoSessionActor`.
  */
sealed trait CryptoSessionAPI

/**
  * An announcement to end the current session.
  */
final case class DropCryptoSession() extends CryptoSessionAPI
