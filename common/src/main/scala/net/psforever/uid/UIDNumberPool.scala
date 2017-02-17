// Copyright (c) 2016 PSForever.net to present
package net.psforever.uid

/**
  * A trait that should be common to all classes that manage a `UIDSource`.
  * @tparam E the `type` of the "purpose" field
  */
trait UIDNumberPool[E] {
  /**
    * Request a certain number of UIDs to be issued.
    * @param n the number of UID keys requested
    * @return a `List` of the UID keys that have been issued
    */
  def getAvailableUID(n : Int) : List[LoanedKey]

  /**
    * Request a specific UID to be issued.
    * @param uid a specific UID
    * @return a UID key, if the requested UID could be produced; `None, otherwise`
    */
  def getSpecificAvailableUID(uid : Int) : Option[LoanedKey]

  /**
    * Return specific UIDs back to the control of their source.
    * @param list a `List` of the UIDs to return
    * @return a `List` of "purpose" objects
    */
  def returnUsedUID(list : List[Int]) : List[E]

  /**
    * Reuse these specific UIDs if they are currently issued.
    * @param list a `List` of the UIDs to reuse
    * @return a `List` of paired UID keys
    */
  def reissueUsedUID(list : List[Int]) : List[LoanedKey]

  /**
    * Disallow these specific UIDs from being issued.
    * @param list a `List` of the UIDs to prohibit
    * @return a `List` of the UID keys whose with UIDs were prohibited
    */
  def restrictAvailableUID(list : List[Int]) : List[LoanedKey]

  /**
    * Allow these specific UIDs to be issued if they had previously been disallowed.
    * @param list a `List` of the UIDs to allow again
    * @return a `List` of "purpose" objects
    */
  def allowRestrictedUID(list : List[Int]) : List[E]

  /**
    * Request the "purpose" attached to a specific UID.
    * @param key a specific UID
    * @return an object suggesting how this UID is being used, if it is being used
    */
  def getObjectFromUID(key : Int) : Option[E]

  /**
    * Stop this manager from acting upon the UID source.
    * @return always return `Nil`
    */
  def end() : List[E]
}
