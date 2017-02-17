// Copyright (c) 2016 PSForever.net to present
package net.psforever.uid

/**
  * A `LoanedKey` is like a `WeakReference` to a UID key.
  * UID keys are only stored by the originating key generator object.
  * A "loaned key" can travel apart from its generator without exposing the original key entry to excess mutability.
  * @param uid the UID represented by this indirect key
  * @param _key a private reference to the original key from the `UIDGen` object
  */
case class LoanedKey(uid : Int, private val _key : UIDSource.UidKey) {
  /**
    * Reference the current `AvailabilityPolicy` of the original UID key
    * @return the `AvailabilityPolicy`
    */
  def policy : AvailabilityPolicy.Value = {
    _key.policy
  }

  /**
    * Reference the current "purpose" of the original UID key.
    * @return an object suggesting how this UID is being used, if it is being used
    */
  def obj : Option[Any] = {
    _key.obj
  }

  /**
    * Set the "purpose" object of the original UID key.
    * This is the only mutable exposure given to the original UID key entry from its "loaned" copy.<br>
    * <br>
    * The primary purpose is to facilitate access to an underlying object reference that suggests the UID's purpose.
    * It is considered fail-safe.
    * The field is only set when the actual key is considered `LEASED` and the field has not been previously set.
    * Those are appropriate conditions.
    * @param obj an object suggesting how this UID will be used
    * @return `true`, if the purpose reference was set; `false`, in any other case
    */
  def obj(obj : Any) : Boolean = {
    if(_key.policy == AvailabilityPolicy.LEASED && _key.obj.isEmpty) {
      _key.obj = Some(obj)
      true
    }
    else {
      false
    }
  }
}

