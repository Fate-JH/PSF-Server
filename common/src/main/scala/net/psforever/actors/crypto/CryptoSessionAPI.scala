// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.crypto

sealed trait CryptoSessionAPI

final case class DropCryptoSession() extends CryptoSessionAPI
