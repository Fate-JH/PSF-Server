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
    * It does, however, act like it has been `LEASED` and can not be casually be returned.
    */
  val
    AVAILABLE,
    LEASED,
    RESTRICTED
    = Value
}

/**
  * Produce a relatively pseudorandom series of unique identification numbers (UIDs) from a pool of candidate numbers.
  * The UIDs that have been made available are returned as "keys" that reflect the underlying UID's lookup entry.<br>
  * <br>
  * The number of UIDs managed by this object is fixed at compile time.
  * UIDs are selected from one or more accessible pools through one of two pseudorandom drawing processes.
  * The pool and method referred to contains either:
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
  * The keys are an abstract referential object to keep track of the current "purpose" of the UID.
  * Additionally, the key also remembers the current selectable state of its managed UID.
  * Although the policies are fixed, the "purpose" is not a fixed (type of) entity.
  * The only requirement of the "purpose" is that it properly reflects the current application.<br>
  * <br>
  * When UIDs are selected, but have not been used, they should be returned immediately to avoid artificial depletion.
  * @param keys the number of UIDs to be managed by this resource;
  *             must be a positive integer or zero;
  *             the actual UIDs generated is exclusive, from `0 -- (keys-1)`
  * @throws IllegalArgumentException if the number of UIDs managed is less than one
  */
class UIDGen(keys: Int) {
  private val log = org.log4s.getLogger
  if(keys < 1) {
    log.error("UID distribution asked to produce a non-positive number of keys")
    throw new IllegalArgumentException("UID distribution asked to produce a non-positive number of keys")
  }

  import scala.collection.mutable
  import scala.collection.mutable.ListBuffer
  /**
    * An `Array` of the UID monitors - keys - managed by this resource.
    */
  private val keyring : Array[UIDGen.UidKey] = Array.ofDim[UIDGen.UidKey](keys)
  /**
    * A `Queue` of all UID keys that have been issued and returned at some point, and are available for re-issuing.
    */
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
  def getAvailableUID(n : Int = 1) : List[LoanedKey] = {
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
  def getSpecificAvailableUID(uid : Int) : Option[LoanedKey] = {
    var outKey : Option[LoanedKey] = None
    if(-1 < uid && uid < keys) {
      val uidKey : UIDGen.UidKey = keyring(uid)
      if(uidKey.policy == AvailabilityPolicy.AVAILABLE) {
        uidKey.policy = AvailabilityPolicy.LEASED
        outKey = Some(LoanedKey(uid, uidKey))
      }
    }
    outKey
  }

  /**
    * Return specific UIDs back to the control of their generator.<br>
    * <br>
    * When the UID is returned, it is no longer considered attached to its previous "purpose."
    * The UID key is marked as `AVAILABLE` if it was `LEASED`, and its "purpose" is returned.
    * For each UID, its "purpose" is returned in that same order for comparison.
    * If the UID is invalid, it returns `None` in the same place.
    * A `RESTRICTED` UID returns `None` as well as it can not be returned at all.
    * @param list a `List` of the UIDs to return
    * @return a `List` of "purpose" objects
    */
  def returnUsedUID(list : List[Int]) : List[Any] = {
    val outList : mutable.ListBuffer[Any] = new mutable.ListBuffer[Any]
    list.foreach { uid : Int =>
      var out : Any = None
      if(-1 < uid && uid < keys) {
        val uidKey : UIDGen.UidKey = keyring(uid)
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
  def reissueUsedUID(list : List[Int]) : List[LoanedKey] = {
    val outList : mutable.ListBuffer[LoanedKey] = new mutable.ListBuffer[LoanedKey]
    list.foreach { uid : Int =>
      if(-1 < uid && uid < keys) {
        val uidKey : UIDGen.UidKey = keyring(uid)
        if(uidKey.policy == AvailabilityPolicy.LEASED) {
          val newKey = new UIDGen.UidKey()
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
  def restrictAvailableUID(list : List[Int]) : List[LoanedKey] = {
    val outList : mutable.ListBuffer[LoanedKey] = new mutable.ListBuffer[LoanedKey]
    list.foreach { uid : Int =>
      if(-1 < uid && uid < keys) {
        val uidKey : UIDGen.UidKey = keyring(uid)
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
  def allowRestrictedUID(list : List[Int]) : List[Any] = {
    val outList : mutable.ListBuffer[Any] = new mutable.ListBuffer[Any]
    list.foreach { uid : Int =>
      var out : Any = None
      if(-1 < uid && uid < keys) {
        val uidKey : UIDGen.UidKey = keyring(uid)
        if(uidKey.policy == AvailabilityPolicy.RESTRICTED) {
          uidKey.policy = AvailabilityPolicy.LEASED
          out = uidKey.obj.get
        }
      }
      outList += out
    }
    outList.toList
  }

  /**
    * Request the "purpose" attached to a specific UID.
    * @param key a specific UID
    * @return an object suggesting how this UID is being used, if it is being used
    */
  def getObjectFromUID(key : Int) : Option[Any] = {
    val uidKey : UIDGen.UidKey = keyring(key)
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
    /*
    The following process selects random Integers from a sorted List without discarding elements or repeating elements.
    The list is traversed in-order, starting at a current index position.
    A random element is chosen from the range of numbers between the index-position and the end of the list.
    The index position element is then swapped with the chosen element so that the former remains available for selection.
    The current index position is progressed per attempt, constraining the span of elements that were not (yet) chosen.
     */
    while(permuteNum <= keys && cnt < num) {
      var policy : AvailabilityPolicy.Value = AvailabilityPolicy.LEASED //any policy but AVAILABLE
      var out : Int = -1
      while(permuteNum <= keys && policy != AvailabilityPolicy.AVAILABLE) {
        val n : Int = rand.nextInt(keys - permuteNum) //call a random entry still in the list
        out = initialChain(permuteNum + n)
        policy = keyring(out).policy
        initialChain(permuteNum + n) = initialChain(permuteNum) //swap the index-position element with the random element
        permuteNum += 1
      }

      if(policy == AvailabilityPolicy.AVAILABLE) { //only return AVAILABLE UID keys
        val uidKey = keyring(out)
        uidKey.policy = AvailabilityPolicy.LEASED
        outList += LoanedKey(out, uidKey)
        cnt += 1
      }
    }

    if(permuteNum == keys) { //this list is exhausted; clear it for memory and to begin next selection method
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
      val uidKey : UIDGen.UidKey = keyring(index)
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
    for(x <- 0 until keys) {
      keyring(x) = new UIDGen.UidKey
      initialChain += x
    }
  }

  /**
    * Stop this UID manager from acting upon any UIDs.<br>
    * <br>
    * All internal lists are un-allocated.
    * The permanent list of UID keys have any remembered "purposes" cleared.
    * That list of "purposes" is also returned to the caller for any necessary final processing.
    * @return always return `Nil`
    */
  def end() : List[Any] = {
    initialChain.clear
    chain.clear
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

object UIDGen {
  /**
    * A "key" used to hold the conditions for the UID it represents.
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

/**
  * A `LoanedKey` is like a `WeakReference` to a UID key.
  * UID keys are only stored by the originating key generator object.
  * A "loaned key" can travel apart from its generator without exposing the original key entry to excess mutability.
  * @param uid the UID represented by this indirect key
  * @param _key a private reference to the original key from the `UIDGen` object
  */
case class LoanedKey(uid : Int, private val _key : UIDGen.UidKey) {
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
