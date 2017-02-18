// Copyright (c) 2016 PSForever.net to present
package net.psforever.uid

/**
  * na
  * @param src the object that governs the unique numbers
  */
class InOrderSelector(src : UniqueIdentifierSource) extends UniqueIdentifierSelector(src) {
  private val log = org.log4s.getLogger

  import scala.collection.mutable
  protected val chain : mutable.Queue[Int] = new mutable.Queue[Int]
  setup()

  /**
    * Request a certain number of UIDs.
    * Only UIDs that are `AVAILABLE` are selected and set properly to `LEASED` before being returned for utilization.<br>
    * <br>
    * The UIDs tested for issuing with this method return in ascending numeric order, at least initially.
    * Once the last number is consumed, the list starts over from the first element and begins testing them in ascending order again.
    * @param num the number of UID keys requested;
    *            defaults to one (1)
    * @return a `List` of the UID keys that have been issued
    */
  override def getAvailable(num : Int) : List[LoanedKey] = {
    val outList : mutable.ListBuffer[LoanedKey] = new mutable.ListBuffer[LoanedKey]
    var cnt : Int = 0
    while(cnt < num && chain.nonEmpty) {
      val index : Int = chain.dequeue //always `dequeue` the head element
      val uidKey : NumberSource.UidKey = src.key(index)
      if(uidKey.policy == AvailabilityPolicy.AVAILABLE) { //only return AVAILABLE UID keys
        uidKey.policy = AvailabilityPolicy.LEASED
        outList += LoanedKey(index, uidKey)
        cnt += 1
      }
    }
    if(outList.isEmpty) {
      log.warn("All keys currently in use")
    }
    outList.toList
  }

  /**
    * Return specific UIDs back to the control of their source.<br>
    * <br>
    * When the UID is returned, it is no longer considered attached to its previous "purpose."
    * The UID key is marked as `AVAILABLE` if it was `LEASED`, and its "purpose" is returned.
    * For each UID, its "purpose" is returned in that same order for comparison.
    * If the UID is invalid, it returns `None` in the same place.
    * A `RESTRICTED` UID returns `None` as well as it can not be returned at all.<br>
    * <br>
    * When UIDs are selected, but have not been used, they should be returned immediately to avoid artificial depletion.
    * @param list a `List` of the UIDs to return
    * @return a `List` of "purpose" objects
    */
  override def returnUsed(list : List[Int]) : List[Any] = {
    val outList : mutable.ListBuffer[Any] = new mutable.ListBuffer[Any]
    val last : Int = getLast
    val first = getFirst - 1
    list.foreach { uid : Int =>
      var out : Any = None
      if(first < uid && uid < last) {
        val uidKey : NumberSource.UidKey = src.key(uid)
        if(uidKey.policy == AvailabilityPolicy.LEASED) {
          uidKey.policy = AvailabilityPolicy.AVAILABLE
          out = uidKey.obj.get
          uidKey.obj = None
          chain.enqueue(uid)
        }
      }
      outList += out
    }
    outList.toList
  }

  /**
    * Initialize the maintained `Queue` of UID keys with individual key objects.
    * Add all UIDs, by index, to a mutable `List` for the selection process of the "first batch."
    */
  def setup() : Unit = {
    val last : Int = getLast
    val first : Int = getFirst
    for(n <- first until last) {
      chain += n
    }
  }

  /**
    * Stop this selector from acting upon the UID source and perform cleanup.
    * @return always returns `Nil`
    */
  override def end() : List[Any] = {
    chain.clear
    Nil
  }
}
