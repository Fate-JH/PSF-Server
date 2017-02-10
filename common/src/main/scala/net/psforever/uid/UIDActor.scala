// Copyright (c) 2016 PSForever.net to present
package net.psforever.uid

final case class ReadyToSelectUIDs()
final case class RestrictedUID(list : List[Int])
final case class ReturnUsedUID(list : List[Int])
final case class ReissueUsedUID(list : List[Int])
final case class RequestAvailableUID(n : Int = 1)
final case class GetObjectFromUID(key : Int)

/**
  * Enforce synchronization on an underlying UID source object.
  * Recommended use is the `akka` `ask` pattern.
  * @param uidSource the manager for a series of UIDs
  */
class UIDActor(private val uidSource : UIDGen) extends akka.actor.Actor {
  //private val log = org.log4s.getLogger

  /**
    * The initial behavior demonstrated by this `Actor`.
    * Pass to the method `initialize`.
    * @return a partial function
    */
  def receive : Receive = Initializing

  /**
    * The initial behavior demonstrated by this `Actor`.
    * UIDs can only be restricted from future selection for now.
    * There is no potential for interaction with request and return operations until the `Actor` is instructed.
    * Passing a `ReadyToSelectUIDs` message does not require the `ask` pattern.
    * @return a partial function
    * @see `Started`
    */
  def Initializing : Receive = {
    case ReadyToSelectUIDs =>
      context.become(Started)

    case RestrictedUID(list : List[Int]) =>
      sender ! uidSource.restrictAvailableUID(list)

    case GetObjectFromUID(key : Int) =>
      sender ! uidSource.getObjectFromUID(key)

    case _ =>
      //nothing here
  }

  /**
    * After the initial function has been successfully progressed, this behavior is moved to the top of the stack.
    * Subsequent activity is handled by the guidelines of this behavior.
    * UIDs may no longer be restricted from selection permanent; but, they may undergo the normal selection processes.
    * @return a partial function
    */
  def Started : Receive = {
    case RequestAvailableUID(n : Int) =>
      sender ! uidSource.getAvailableUID(n)

    case ReturnUsedUID(list : List[Int]) =>
      sender ! uidSource.returnUsedUID(list)

    case ReissueUsedUID(list : List[Int]) =>
      sender ! uidSource.reissueUsedUID(list)

    case GetObjectFromUID(key : Int) =>
      sender ! uidSource.getObjectFromUID(key)

    case _ =>
    //nothing here
  }
}
