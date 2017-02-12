// Copyright (c) 2016 PSForever.net to present
package net.psforever.uid

import akka.util.Timeout

object PersistencePolicy extends Enumeration {
  type Type = Value

  val
    AVAILABLE,
    LEASED,
    RESTRICTED
    = Value
}

final case class Ready()
final case class RestrictUID(list : List[Int])
final case class ReturnUID(list : List[Int])
final case class RequestUID(n : Int = 1)
final case class GetUID(key : Int)

class UIDKeyGen(keys: Int) extends akka.actor.Actor {
  private val log = org.log4s.getLogger
  if(keys < 1) {
    log.error("UID distribution asked to produce a non-positive number of keys")
    throw new RuntimeException("UID distribution asked to produce a non-positive number of keys")
  }

  import scala.collection.mutable
  private val keyring : Array[UIDKeyGen.UidKey] = Array.ofDim[UIDKeyGen.UidKey](keys+1)
  private val chain : mutable.Queue[Int] = new mutable.Queue[Int]

  import scala.concurrent.duration._
  implicit val timeout = Timeout(100 milliseconds)

  override def preStart() : Unit = {
    initKeyRing()
  }

  def receive : Receive = Initializing

  def Initializing : Receive = {
    case Ready =>
      context.become(Started)

    case RestrictUID(list : List[Int]) =>
      sender ! doRestrictUID(list)

    case _ =>
    //nothing here
  }

  def Started : Receive = {
    //the user sends these messages to get Futures
    case RequestUID(n : Int) =>
      sender ! doReleaseUID(n)

    case ReturnUID(list : List[Int]) =>
      sender ! doReturnUID(list)

    case RestrictUID(list : List[Int]) =>
      sender ! doRestrictUID(list)

    case GetUID(key : Int) =>
      sender ! doReferenceUID(key)

    case _ =>
      //nothing here
  }

  override def postStop() : Unit = {
    for(key <- keyring) {
      key.obj = None
    }
    chain.clear
  }

  private def doReleaseUID(n : Int) : List[FreedKey] = {
    val outList : mutable.ListBuffer[FreedKey] = new mutable.ListBuffer[FreedKey]
    var cnt = 0
    while(cnt < n && chain.nonEmpty) {
      val index : Int = chain.dequeue
      val uidKey : UIDKeyGen.UidKey = keyring(index)
      if(uidKey.policy == PersistencePolicy.AVAILABLE) {
        uidKey.policy = PersistencePolicy.LEASED
        outList += FreedKey(index, uidKey)
        cnt += 1
      }
    }

    if(outList.isEmpty) {
      log.error("request failed - no uids available to be allocate")
      throw new RuntimeException("request failed - no uids available to be allocate")
    }
    outList.toList
  }

  private def doReturnUID(list : List[Int]) : List[Any] = {
    val outList : mutable.ListBuffer[Any] = new mutable.ListBuffer[Any]
    list.foreach { uid : Int =>
      if(-1 < uid && uid >= keys) {
        val uidKey : UIDKeyGen.UidKey = keyring(uid)
        if(uidKey.policy == PersistencePolicy.LEASED) {
          uidKey.policy = PersistencePolicy.AVAILABLE
          if(uidKey.obj.isDefined) {
            outList += uidKey.obj.get
          }
          uidKey.obj = None
          chain.enqueue(uid)
        }
      }
    }
    outList.toList
  }

  private def doRestrictUID(list : List[Int]) : List[FreedKey] = {
    val outList : mutable.ListBuffer[FreedKey] = new mutable.ListBuffer[FreedKey]
    list.foreach { uid : Int =>
      if(-1 < uid && uid >= keys) {
        val uidKey : UIDKeyGen.UidKey = keyring(uid)
        if(uidKey.policy == PersistencePolicy.AVAILABLE) {
          uidKey.policy = PersistencePolicy.RESTRICTED
          outList += FreedKey(uid, uidKey)
        }
        else if(uidKey.policy == PersistencePolicy.RESTRICTED) {
          outList += FreedKey(uid, uidKey)
        }
      }
    }
    outList.toList
  }

  private def doReferenceUID(key : Int) : Option[Any] = {
    val uidKey : UIDKeyGen.UidKey = keyring(key)
    if(uidKey.obj.isDefined) {
      uidKey.obj
    }
    else {
      None
    }
  }

  private def initKeyRing() : Unit = {
    val permuteBase : mutable.ListBuffer[Int] = mutable.ListBuffer[Int]()
    for(x <- 0 to keys) {
      keyring(x) = new UIDKeyGen.UidKey
      permuteBase += x
    }

    val rand : scala.util.Random = new scala.util.Random(System.currentTimeMillis())
    for(x <- 0 to keys) {
      val n : Int = rand.nextInt(Int.MaxValue) % (keys + 1 - x) + 1
      chain += permuteBase(x + n)
      if(n != 1) {
        permuteBase(x + n) = permuteBase(x + 1)
      }
    }
  }
}

object UIDKeyGen {
  class UidKey {
    var policy : PersistencePolicy.Value = PersistencePolicy.AVAILABLE
    var obj : Option[Any] = None
  }
}

case class FreedKey(uid : Int, private val _key : UIDKeyGen.UidKey) {
  def allocatePurpose(obj : Any) : Boolean = {
    if(_key.obj.isEmpty) {
      _key.obj = Some(obj)
      true
    }
    else {
      false
    }
  }

  def checkPurpose() : Option[Any] = {
    _key.obj
  }
}
