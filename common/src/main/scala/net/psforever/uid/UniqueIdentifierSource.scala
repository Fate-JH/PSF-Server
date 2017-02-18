// Copyright (c) 2016 PSForever.net to present
package net.psforever.uid

import scala.collection.mutable

/**
  * For a continuous series of numbers, produce a relatively continuous unique identifiers to be managed.
  * The UIDs that have been made available are returned as "keys".<br>
  * <br>
  * The number of UIDs managed by this object is fixed at compile time.
  * The keys are an abstract referential object to keep track of the current "purpose" of the UID.
  * Additionally, the key also remembers the current selectable state of its managed UID.
  * Although the policies are fixed, the "purpose" is not a fixed (type of) entity.
  * The only requirement of the "purpose" is that it properly reflects the current application.
  * @param keys the number of UIDs to be managed by this resource;
  *             must be a positive integer or zero;
  *             the actual UIDs generated is exclusive, from `0 -- (keys-1)`
  * @throws IllegalArgumentException if the number of UIDs managed is less than one
  */
class UniqueIdentifierSource(keys : Int) {
  //private val log = org.log4s.getLogger
  if(keys < 1) {
    throw new IllegalArgumentException("UID distribution asked to produce a non-positive number of keys")
  }

  /**
    * An `Array` of the UID monitors - keys - managed by this resource.
    */
  private val keyring : Array[NumberSource.UidKey] = Array.ofDim[NumberSource.UidKey](keys)
  initKeyRing()

  /**
    * Initialize the perfectly-maintained `Queue` of UID keys with individual key objects.
    */
  private def initKeyRing() : Unit = {
    for(x <- 0 until keys) {
      keyring(x) = new NumberSource.UidKey
    }
  }

  /**
    * Get the total number of UIDs.
    * @return the total count of UIDs managed by this resource
    */
  def size : Int = {
    keyring.length
  }

  /**
    * na
    * @param uid the requested UID
    * @throws IndexOutOfBoundsException if the requested uid is above or below the range
    * @return
    */
  def key(uid : Int) : NumberSource.UidKey = {
    if(-1 < uid && uid > keyring.length) {
      throw new IndexOutOfBoundsException(s"this source does not track a UID $uid")
    }
    keyring(uid)
  }

  /**
    * Stop this source from maintaining any UIDs.<br>
    * <br>
    * The permanent list of UID keys have any remembered "purposes" cleared.
    * That list of "purposes" is also returned to the caller for any necessary final processing.
    * @return a `List` of "purposes"
    */
  def end() : List[Any] = {
    val outList : mutable.ListBuffer[Any] = new mutable.ListBuffer[Any]
    for(x <- 0 until keys) {
      if(keyring(x).obj.isDefined) {
        outList += keyring(x).obj.get
        keyring(x).obj = None
      }
    }
    outList.toList
  }
}

object NumberSource {
  /**
    * A "key" used to hold the conditions for the unique number it is intended to represent.
    */
  class UidKey {
    /**
      * Represent the current and future usage state of this UID.
      */
    var policy : AvailabilityPolicy.Value = AvailabilityPolicy.AVAILABLE
    /**
      * What foreign entity is using this UID, if applicable.
      * This can not be a WeakReference-like field.
      */
    var obj : Option[Any] = None
  }
}
