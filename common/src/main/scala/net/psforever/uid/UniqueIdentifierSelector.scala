// Copyright (c) 2016 PSForever.net to present
package net.psforever.uid

import scala.collection.mutable

/**
  * Base of all classes that manage a `UniqueIdentifierSource`.
  * @param src the object that governs the unique numbers
  */
abstract class UniqueIdentifierSelector(private val src : UniqueIdentifierSource) {
  /**
    * Get the start of the valid selection of numbers.
    * @return the first key's index;
    *         by default, 0
    */
  def getFirst : Int = {
    0
  }

  /**
    * Get the end of the valid selection of numbers.
    * This should be overrode by an implementing class.
    * @return the last key's index;
    *         by default, the size of the `src`
    */
  def getLast : Int = {
    src.size
  }

  /**
    * Request a certain number of UIDs to be issued.
    * @param num the number of UID keys requested
    * @return a `List` of the UID keys that have been issued
    */
  def getAvailable(num : Int) : List[LoanedKey]

  /**
    * Request a specific UID to be issued.
    * @param uid a specific UID
    * @return a UID key, if the requested UID could be produced; `None, otherwise`
    */
  def getSpecificAvailable(uid : Int) : Option[LoanedKey] = {
    UniqueIdentifierSelector.getSpecificAvailable(uid, src)
  }

  /**
    * Return specific UIDs back to the control of their source.<br>
    * <br>
    * When UIDs are selected, but have not been used, they should be returned immediately to avoid artificial depletion.
    * @param list a `List` of the UIDs to return
    * @return a `List` of "purpose" objects
    */
  def returnUsed(list : List[Int]) : List[Any]

  /**
    * Reuse these specific UIDs if they are currently issued.
    * @param list a `List` of the UIDs to reuse
    * @return a `List` of paired UID keys
    */
  def reissueUsed(list : List[Int]) : List[LoanedKey] = {
    UniqueIdentifierSelector.reissueUsed(list, src)
  }

  /**
    * Disallow these specific UIDs from being issued.
    * @param list a `List` of the UIDs to prohibit
    * @return a `List` of the UID keys whose with UIDs were prohibited
    */
  def restrictAvailable(list : List[Int]) : List[LoanedKey] = {
    UniqueIdentifierSelector.restrictAvailable(list, src)
  }

  /**
    * Allow these specific UIDs to be issued if they had previously been disallowed.
    * @param list a `List` of the UIDs to allow again
    * @return a `List` of "purpose" objects
    */
  def allowRestricted(list : List[Int]) : List[Any] = {
    UniqueIdentifierSelector.allowRestricted(list, src)
  }

  /**
    * Request the "purpose" attached to a specific UID.
    * @param uid a specific UID
    * @return an object suggesting how this UID is being used, if it is being used
    */
  def getObjectFrom(uid : Int) : Option[Any] = {
    UniqueIdentifierSelector.getObjectFrom(uid, src)
  }

  /**
    * Stop this manager from acting upon the UID source.
    * @return by default, `Nil`
    */
  def end() : List[Any] = Nil
}

object UniqueIdentifierSelector {
  /**
    * Request a specific UID.
    * Only a UID that is `AVAILABLE` is selected and properly set to `LEASED` before being returned for utilization.<br>
    * <br>
    * This method does not worry about the established UID selection methods as we already know what it is.
    * Furthermore, conditions during the normal selection methods will rule out any UID that is encountered and `LEASED`.
    * A "unexpectedly `LEASED`" UID will not impact any normal issuing order.
    * @param uid a specific UID
    * @return a UID key, if the requested UID could be produced; `None, otherwise`
    */
  def getSpecificAvailable(uid : Int, src : UniqueIdentifierSource) : Option[LoanedKey] = {
    var outKey : Option[LoanedKey] = None
    if((-1) < uid && uid < src.size) {
      val uidKey : NumberSource.UidKey = src.key(uid)
      if(uidKey.policy == AvailabilityPolicy.AVAILABLE) {
        uidKey.policy = AvailabilityPolicy.LEASED
        outKey = Some(LoanedKey(uid, uidKey))
      }
    }
    outKey
  }

  /**
    * Reuse these specific UIDs if they are currently issued.<br>
    * <br>
    * If a given UID is already in use, its "purpose" will be forgotten and it can be assigned a new "purpose."
    * This is represented by a returned `LoanedKey` that is paired with the `LoanedKey` following it.
    * The first one lists the original "purpose" of the UID.
    * The second one can be used for reassigning the "purpose."
    * @param list a `List` of the UIDs to reuse
    * @return a `List` of paired UID keys
    */
  def reissueUsed(list : List[Int], src : UniqueIdentifierSource) : List[LoanedKey] = {
    val outList : mutable.ListBuffer[LoanedKey] = new mutable.ListBuffer[LoanedKey]
    val last : Int = src.size
    val first = -1
    list.foreach { uid : Int =>
      if(first < uid && uid < last) {
        val uidKey : NumberSource.UidKey = src.key(uid)
        if(uidKey.policy == AvailabilityPolicy.LEASED) {
          val newKey = new NumberSource.UidKey()
          newKey.obj = uidKey.obj
          outList += LoanedKey(uid, newKey)
          uidKey.obj = None
          outList += LoanedKey(uid, uidKey)
        }
      }
    }
    outList.toList
  }

  /**
    * Disallow these specific UIDs from being selected by normal draw.<br>
    * <br>
    * The only UIDs to be marked as `RESTRICTED` are those that would impact stability in the rest of the system were they selected.
    * Only a UID that is `AVAILABLE` may be marked as `RESTRICTED` to avoid misappropriating UIDs.
    * (If any UID that is already `RESTRICTED` is encountered, it is also returned.)
    * @param list a `List` of the UIDs to prohibit
    * @return a `List` of the UID keys whose with UIDs were prohibited
    */
  def restrictAvailable(list : List[Int], src : UniqueIdentifierSource) : List[LoanedKey] = {
    val outList : mutable.ListBuffer[LoanedKey] = new mutable.ListBuffer[LoanedKey]
    val last : Int = src.size
    val first = -1
    list.foreach { uid : Int =>
      if(first < uid && uid < last) {
        val uidKey : NumberSource.UidKey = src.key(uid)
        if(uidKey.policy == AvailabilityPolicy.AVAILABLE) {
          uidKey.policy = AvailabilityPolicy.RESTRICTED
          outList += LoanedKey(uid, uidKey)
        }
        else if(uidKey.policy == AvailabilityPolicy.RESTRICTED) {
          outList += LoanedKey(uid, uidKey)
        }
      }
    }
    outList.toList
  }

  /**
    * Allow these specific UIDs to be selected by random draw if they had previously been disallowed.<br>
    * <br>
    * For safety, a re-allowed UID is changed from an `AvailabilityPolicy` of `RESTRICTED` to `LEASED`.
    * The UID will not be returned to the pool of random draw UIDs like this.
    * The method does not return `LoanedKey`s since those allow for re-use if the "purpose" is not defined.
    * To properly repurpose the UID, it has to be returned after being re-allowed.<br>
    * <br>
    * If the UID is invalid, it returns `None` in the same place in the `List` of "purposes."
    * @param list a `List` of the UIDs to allow again
    * @return a `List` of "purpose" objects
    */
  def allowRestricted(list : List[Int], src : UniqueIdentifierSource) : List[Any] = {
    val outList : mutable.ListBuffer[Any] = new mutable.ListBuffer[Any]
    val last : Int = src.size
    val first = -1
    list.foreach { uid : Int =>
      var out : Any = None
      if(first < uid && uid < last) {
        val uidKey : NumberSource.UidKey = src.key(uid)
        if(uidKey.policy == AvailabilityPolicy.RESTRICTED) {
          uidKey.policy = AvailabilityPolicy.LEASED
          out = uidKey.obj.get
        }
      }
      outList += out
    }
    outList.toList
  }

  def getObjectFrom(uid : Int, src : UniqueIdentifierSource) : Option[Any] = {
    val uidKey : NumberSource.UidKey = src.key(uid)
    if(uidKey.obj.isDefined) {
      uidKey.obj
    }
    else {
      None
    }
  }
}
