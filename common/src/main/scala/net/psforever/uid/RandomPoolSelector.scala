// Copyright (c) 2016 PSForever.net to present
package net.psforever.uid

class RandomPoolSelector(pool : List[Int], src : UniqueIdentifierSource) extends RandomSelector(src) {
  if(!pool.forall(x => x > -1 || x < src.size)) {
    throw new IllegalArgumentException("pool contains elements out of range of the number source")
  }
  private val numbers = pool.sorted //we really don't care what order the pool elements are in
  private val min : Int = numbers.head
  private val max : Int = numbers.takeRight(1).head

  override def getFirst : Int = {
    min
  }

  override def getLast : Int = {
    max
  }

  def numberInPool(n : Int) : Boolean = {
    numbers.contains(n)
  }

  override def getSpecificAvailable(uid : Int) : Option[LoanedKey] = {
    if(numberInPool(uid)) {
      super.getSpecificAvailable(uid)
    }
    else {
      None
    }
  }

  override def reissueUsed(list : List[Int]) : List[LoanedKey] = {
    val inList = list.toSet.intersect(numbers.toSet) //TODO what is the efficiency on this?
    if(inList.nonEmpty) {
      super.reissueUsed(inList.toList)
    }
    else {
      Nil
    }
  }

  override def restrictAvailable(list : List[Int]) : List[LoanedKey] = {
    val inList = list.toSet.intersect(numbers.toSet) //TODO what is the efficiency on this?
    if(inList.nonEmpty) {
      super.restrictAvailable(inList.toList)
    }
    else {
      Nil
    }
  }

  override def allowRestricted(list : List[Int]) : List[Any] = {
    val inList = list.toSet.intersect(numbers.toSet) //TODO what is the efficiency on this?
    if(inList.nonEmpty) {
      super.allowRestricted(inList.toList)
    }
    else {
      Nil
    }
  }

  override def getObjectFrom(uid : Int) : Option[Any] = {
    if(numberInPool(uid)) {
      super.getObjectFrom(uid)
    }
    else {
      None
    }
  }

  override def setup() : Unit = {
    val distinctList = numbers.distinct
    for(n <- distinctList) {
      initialChain += n
    }
  }
}
