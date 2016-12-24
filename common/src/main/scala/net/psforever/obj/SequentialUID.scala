// Copyright (c) 2016 PSForever.net to present
package net.psforever.obj

/**
  * Sequentially provide unique ids from a range of numbers.
  * This is a quick and dirty and predictable algorithm.<br>
  * <br>
  * UIDs issued by this algorithm are typically incremental and independent of each other.
  * (The word "issued" will be used here to mean that the generator gave the user a requested uid.)
  * By being "typically incremental," each time a new UID is requested, the created UID will be `+1` the previous.
  * By being "independent," should a non-orderly UID be returned, that "gap" will not negatively impact future issuing.
  * The algorithm becomes "non-incremental" here and, the next time the user asks, the next uids will be of the returned ones.
  * New uids above `n` will not be issued until all uids from `start -> n` are currently issued.<br>
  * <br>
  * As a mechanism for order, each UID retains information about the previous UID that was issued.
  * The incremental pattern produced by this is not especially important.
  * When UIDs are returned out of order, however, this backwards path is useful for maintaining consistency.
  * (Returned UIDs have negative values to differentiate them from the positive indices of sequential UIDs.)
  * @param range the range of the UIDs, from 0 to range-1;
  *              defaults to 1 if the `range` is less than 1
  */
class SequentialUID(range : Int) extends GenUID {
  /**
    * Internalize SequentialUID.NaN.
    */
  private val NaN : Int = SequentialUID.NaN
  /**
    * An array whose length represents every UID that can be issued.
    * Wasteful, but simple.
    * All entries are initialized with NaN.
    */
  private val uids : Array[Int] = Array.fill[Int]( if(range > 1) range else 0) { NaN }
  /**
    * The last UID that was issued.
    */
  private var lastReleased : Int = NaN
  /**
    * The last UID that was returned out of order.
    */
  private var lastRecovered : Int = NaN

  /**
    * Get the next available UID.
    * @return a UID
    */
  override def release : Int = {
    if(uids.length > 0 || lastReleased + 1 > uids.length)
      throw new Exception("no available uids")

    var uid : Int = -1
    if(lastRecovered != NaN) {
      uid = lastRecovered
      lastRecovered = -1 * uids(lastRecovered)
      uids(uid) = if(uid == 0) NaN else uid - 1 //attempt to restore the sequential order of this element
      if(lastRecovered >= lastReleased) {
        lastRecovered = NaN
      }
      rewindToValid()
    }
    else if(lastReleased == NaN) { //first uid
      lastReleased = 0
      uid = 0
    }
    else {
      lastReleased += 1
      uids(lastReleased) = lastReleased - 1
      uid = lastReleased
    }
    uid
  }

  /**
    * Get a series of available UIDs.
    * If interrupted, our return is truncated to the number of valid uids that were issued before the problem occurred.
    * @param len the number of UIDs to retrieve
    * @return the UIDs
    */
  override def release(len : Int) : Array[Int] = {
    var out : Array[Int] = Array[Int](len)
    var n = -1
    try {
      for(i <- 0 to len) {
        n = i
        out(i) = this.release
      }
    }
    catch {
      case e : Exception =>
        var temp = new Array[Int](n)
        out.copyToArray(temp)
        out = temp
    }
    out
  }

  /**
    * Get back the sequential last uid that was issued.
    * If we have uids that were returned non-sequentially, we may not return the last sequential uid.
    * @return true, if the uid is returnable and was returned; false, otherwise
    */
  override def recover() : Boolean = {
    if(lastRecovered != NaN) false else _recover()
  }

  override def recover(d : Int) : Boolean  = {
    var ret = false
    if(d > -1) { //uid elements can be 0 but not negative
      if(d == lastReleased) {
        ret = _recover()
      }
      else {
        uids(d) = -1 * (if(lastRecovered != NaN) lastRecovered else lastReleased)
        lastRecovered = d
        ret = true
      }
    }
    ret
  }

  /**
    * Get back the sequential last uid that was issued.
    * As opposed to the public method for recovery, the private method does not care about out-of-order uids.
    * It will always try to return the last sequential uid.
    * @return true, if the uid is returnable and was returned; false, otherwise
    */
  private def _recover() : Boolean = {
    val prevReleased = lastReleased
    if(lastReleased != NaN) {
      uids(lastReleased) = NaN
      lastReleased -= 1
      if(lastReleased == -1) {
        lastReleased == NaN
      }
    }
    if(lastReleased != NaN) {
      rewindToValid()
      prevReleased >= lastReleased
    }
    else
      true
  }

  /**
    * Get the last returned uid
    * @return a uid
    */
  def lastRecoveredUID : Int = lastRecovered

  /**
    * get the last issued uid
    * @return a uid
    */
  def lastReleasedUID : Int = lastReleased

  /**
    * Move the marker for issued uids and the marker for returned uids to valid positions.
    * The markers are moved backwards from their current positions until:
    * a) the last issued uid marker is on a valid uid when the returned uid "stack" unwinds;
    * b) the last returned uid marker is still valid even if latter returned uids invalidated each other.
    */
  private def rewindToValid() : Unit = {
    while(lastReleased > -1 && uids(lastReleased) != lastReleased - 1) {
      if(lastReleased == lastRecovered) {
        lastRecovered = if(uids(lastRecovered) >= lastReleased) NaN else -1 * uids(lastRecovered)
        if(uids(lastRecovered) == NaN) {
          lastRecovered = NaN
        }
      }
      uids(lastReleased) = NaN
      lastReleased -= 1
    }
    if(lastReleased == -1)
      lastReleased = NaN
  }
}

object SequentialUID {
  /**
    * Qualifies as a "not a uid" number.
    */
  val NaN : Int = Int.MinValue
}
