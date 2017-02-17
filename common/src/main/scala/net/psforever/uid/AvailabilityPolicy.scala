// Copyright (c) 2016 PSForever.net to present
package net.psforever.uid

/**
  * The availability of individual UIDs is maintained by the given policy.
  */
object AvailabilityPolicy extends Enumeration {
  type Type = Value

  /**
    * An `AVAILABLE` UID is ready and waiting to be `LEASED` for use.
    * A `LEASED` UID has been issued and is currently being used.
    * A `RESTRICTED` UID can never be issued.
    * It should, however, act like it has been `LEASED` and should not be casually be re-permitted.
    */
  val
  AVAILABLE,
  LEASED,
  RESTRICTED
  = Value
}

