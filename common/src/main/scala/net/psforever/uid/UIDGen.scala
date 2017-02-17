// Copyright (c) 2016 PSForever.net to present
package net.psforever.uid

/**
  * Produce a relatively pseudorandom series of unique identification numbers (UIDs) from a pool of candidate numbers.
  * The UIDs that have been made available are returned as "keys" that reflect the underlying UID's lookup entry.<br>
  * <br>
  * The number of UIDs managed by this object is fixed at compile time.
  * UIDs are selected through one of two pseudorandom drawing processes.
  * The process referred to indicates either:
  * a) the UIDs before they have been selected once; and,
  * b) the UIDs after they have been used and returned.
  * A selected UID is provided to the requested context so that its "purpose" can be assigned.
  * A returned UID is flagged by this manager as one that can be placed into the latter pool and no longer has a "purpose."
  * A restricted UID is passed over by the random selection algorithm, though it may have an assigned "purpose."<br>
  * <br>
  * In theory, a UID should be selectable in `O(1)` time.
  * During selection, however, pre-allocated UIDs and restricted UIDs may impede the process.
  * The "first batch" of UIDs will be definitely affected by these categories.
  * After the "first batch," both of these categories will only continue to be an issue if they change during execution.
  * The purpose of the "first batch" is to weed out the irregular unavailable UIDs in a relatively concise manner.
  * Pseudorandomness in the selection process is greatly influenced by the order of being taken out of the "first batch."
  * Further randomness is applied by the FIFO order of UIDs returned to this manager.
  * The smaller the pool of managed numbers, however, the less chance of the randomness affecting the returned values.<br>
  * <br>
  * When UIDs are selected, but have not been used, they should be returned immediately to avoid artificial depletion.
  * @param src the object that governs the UIDs
  */
class UIDGen()(private implicit val src : UIDSource) extends UIDNumberPool[Any] {
  private val log = org.log4s.getLogger

  import scala.collection.mutable
  import scala.collection.mutable.ListBuffer
  private val chain : mutable.Queue[Int] = new mutable.Queue[Int]
  /**
    * The pool of all UIDs from which the "first batch" of keys are selected.
    */
  private val initialChain : mutable.ListBuffer[Int] = mutable.ListBuffer[Int]()
  /**
    * A counter for the number of keys that have been issued from the "first batch."
    */
  private var permuteNum : Int = 0
  initKeyRing()

  /**
    * Request a certain number of UIDs.
    * Only UIDs that are `AVAILABLE` are selected and set properly to `LEASED` before being returned for utilization.<br>
    * <br>
    * This method branches between the methodology used for the "first batch" and the methodology used for every other cycle.
    * If no UIDs are produced, a warning is logged; but, an empty `List` is still returned.
    * @param n the number of UID keys requested;
    *          defaults to one (1)
    * @return a `List` of the UID keys that have been issued
    */
  override def getAvailableUID(n : Int = 1) : List[LoanedKey] = {
    val outList : List[LoanedKey] = (if(initialChain.nonEmpty) { selectNextRandom(n) } else { selectNextIterate(n) }).toList
    if(outList.isEmpty) {
      log.warn("All keys currently in use")
    }
    outList
  }

  /**
    * Request a specific UID.
    * Only a UID that is `AVAILABLE` is selected and properly set to `LEASED` before being returned for utilization.<br>
    * <br>
    * This method does not worry about the established UID selection methods as we already know what it is.
    * Furthermore, conditions during the normal selection methods will rule out any UID that is encountered and `LEASED`.
    * A "unexpectedly `LEASED`" UID will not impact the order.
    * @param uid a specific UID
    * @return a UID key, if the requested UID could be produced; `None, otherwise`
    */
  override def getSpecificAvailableUID(uid : Int) : Option[LoanedKey] = {
    var outKey : Option[LoanedKey] = None
    if(-1 < uid && uid < src.size) {
      val uidKey : UIDSource.UidKey = src.key(uid)
      if(uidKey.policy == AvailabilityPolicy.AVAILABLE) {
        uidKey.policy = AvailabilityPolicy.LEASED
        outKey = Some(LoanedKey(uid, uidKey))
      }
    }
    outKey
  }

  /**
    * Return specific UIDs back to the control of their source.<br>
    * <br>
    * When the UID is returned, it is no longer considered attached to its previous "purpose."
    * The UID key is marked as `AVAILABLE` if it was `LEASED`, and its "purpose" is returned.
    * For each UID, its "purpose" is returned in that same order for comparison.
    * If the UID is invalid, it returns `None` in the same place.
    * A `RESTRICTED` UID returns `None` as well as it can not be returned at all.
    * @param list a `List` of the UIDs to return
    * @return a `List` of "purpose" objects
    */
  override def returnUsedUID(list : List[Int]) : List[Any] = {
    val total : Int = src.size
    val outList : mutable.ListBuffer[Any] = new mutable.ListBuffer[Any]
    list.foreach { uid : Int =>
      var out : Any = None
      if(-1 < uid && uid < total) {
        val uidKey : UIDSource.UidKey = src.key(uid)
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
    * Reuse these specific UIDs if they are currently issued.<br>
    * <br>
    * If a given UID is already in use, its "purpose" will be forgotten and it can be assigned a new "purpose."
    * This is represented by a returned `LoanedKey` that is paired with the `LoanedKey` following it.
    * The first one lists the original "purpose" of the UID.
    * The second one can be used for reassigning the "purpose."
    * @param list a `List` of the UIDs to reuse
    * @return a `List` of paired UID keys
    */
  override def reissueUsedUID(list : List[Int]) : List[LoanedKey] = {
    val total : Int = src.size
    val outList : mutable.ListBuffer[LoanedKey] = new mutable.ListBuffer[LoanedKey]
    list.foreach { uid : Int =>
      if(-1 < uid && uid < total) {
        val uidKey : UIDSource.UidKey = src.key(uid)
        if(uidKey.policy == AvailabilityPolicy.LEASED) {
          val newKey = new UIDSource.UidKey()
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
    * Disallow these specific UIDs from being selected by random draw.<br>
    * <br>
    * The only UIDs to be marked as `RESTRICTED` are those that would impact stability in the rest of the system were they selected.
    * Only a UID that is `AVAILABLE` may be marked as `RESTRICTED` to avoid misappropriating UIDs.
    * (If any UID that is already `RESTRICTED` is encountered, it is also returned.)
    * @param list a `List` of the UIDs to prohibit
    * @return a `List` of the UID keys whose with UIDs were prohibited
    */
  override def restrictAvailableUID(list : List[Int]) : List[LoanedKey] = {
    val total : Int = src.size
    val outList : mutable.ListBuffer[LoanedKey] = new mutable.ListBuffer[LoanedKey]
    list.foreach { uid : Int =>
      if(-1 < uid && uid < total) {
        val uidKey : UIDSource.UidKey = src.key(uid)
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
  override def allowRestrictedUID(list : List[Int]) : List[Any] = {
    val total : Int = src.size
    val outList : mutable.ListBuffer[Any] = new mutable.ListBuffer[Any]
    list.foreach { uid : Int =>
      var out : Any = None
      if(-1 < uid && uid < total) {
        val uidKey : UIDSource.UidKey = src.key(uid)
        if(uidKey.policy == AvailabilityPolicy.RESTRICTED) {
          uidKey.policy = AvailabilityPolicy.LEASED
          out = uidKey.obj.get
        }
      }
      outList += out
    }
    outList.toList
  }

  override def getObjectFromUID(key : Int) : Option[Any] = {
    val uidKey : UIDSource.UidKey = src.key(key)
    if(uidKey.obj.isDefined) {
      uidKey.obj
    }
    else {
      None
    }
  }

  /**
    * A pseudorandom number generator for the "first batch" of UID keys that are issued.
    * Unfortunately, it has no purpose after that "first batch."
    */
  private val rand : scala.util.Random = new scala.util.Random(System.currentTimeMillis())

  /**
    * Select a number of random UID keys from the pool of all `AVAILABLE` UID keys.<br>
    * <br>
    * This method manages the distribution of `AVAILABLE` UID keys during the "first batch."
    * All UIDs start as selections from the "first batch" and are shifted to another list upon being returned.
    * @param num the number of UID keys requested
    * @return a `ListBuffer` of the UID keys that have been issued
    */
  private def selectNextRandom(num : Int) : ListBuffer[LoanedKey] = {
    val outList : mutable.ListBuffer[LoanedKey] = new mutable.ListBuffer[LoanedKey]
    var cnt : Int = 0
    val total : Int = src.size
    /*
    The following process selects random Integers from a sorted List without discarding elements or repeating elements.
    The list is traversed in-order, starting at a current index position.
    A random element is chosen from the range of numbers between the index-position and the end of the list.
    The index position element is then swapped with the chosen element so that the former remains available for selection.
    The current index position is progressed per attempt, constraining the span of elements that were not (yet) chosen.
     */
    while(permuteNum <= total && cnt < num) {
      var policy : AvailabilityPolicy.Value = AvailabilityPolicy.LEASED //any policy but AVAILABLE
      var out : Int = -1
      while(permuteNum <= total && policy != AvailabilityPolicy.AVAILABLE) {
        val n : Int = rand.nextInt(total - permuteNum) //call a random entry still in the list
        out = initialChain(permuteNum + n)
        policy = src.key(out).policy
        initialChain(permuteNum + n) = initialChain(permuteNum) //swap the index-position element with the random element
        permuteNum += 1
      }

      if(policy == AvailabilityPolicy.AVAILABLE) { //only return AVAILABLE UID keys
        val uidKey = src.key(out)
        uidKey.policy = AvailabilityPolicy.LEASED
        outList += LoanedKey(out, uidKey)
        cnt += 1
      }
    }

    if(permuteNum == total) { //this list is exhausted; clear it for memory and to begin next selection method
      initialChain.clear()
      log.info("first batch cleared")
    }
    if(cnt != num ) { //requested number of UIDs not fulfilled yet; peek into next selection method
      outList ++= selectNextIterate(num - cnt)
    }
    outList
  }

  /**
    * Select a number of random UID keys from the pool of all `AVAILABLE` UID keys.
    * After the "first batch" is exhausted, this method will select `AVAILABLE` UID keys from a pool of returned keys.
    * @param num the number of UID keys requested
    * @return a `ListBuffer` of the UID keys that have been issued
    */
  private def selectNextIterate(num : Int) : ListBuffer[LoanedKey] = {
    val outList : mutable.ListBuffer[LoanedKey] = new mutable.ListBuffer[LoanedKey]
    var cnt = 0
    while(cnt < num && chain.nonEmpty) {
      val index : Int = chain.dequeue //always `dequeue` the head element
      val uidKey : UIDSource.UidKey = src.key(index)
      if(uidKey.policy == AvailabilityPolicy.AVAILABLE) { //only return AVAILABLE UID keys
        uidKey.policy = AvailabilityPolicy.LEASED
        outList += LoanedKey(index, uidKey)
        cnt += 1
      }
    }
    outList
  }

  /**
    * Initialize the perfectly-maintained `Queue` of UID keys with individual key objects.
    * Add all UIDs, by index, to a mutable `List` for the selection process of the "first batch."
    */
  private def initKeyRing() : Unit = {
    for(x <- 0 until src.size) {
      initialChain += x
    }
  }

  override def end() : List[Any] = {
    initialChain.clear
    chain.clear
    Nil
  }
}
