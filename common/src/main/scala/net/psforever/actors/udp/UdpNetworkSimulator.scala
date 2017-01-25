// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.udp

import akka.actor.{Actor, ActorRef}
import akka.io.Udp

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.Random
import scala.concurrent.duration._

/**
  * For testing purposes, simulate some message and packet exchanges to be run on this configuration.
  * A significant part of the faking involves pacing the messages rather than a simple "receive to send" protocol.
  * @param server the platform this simulated network will engage
  * @param params artificial conditions which will be simulated
  */
class UdpNetworkSimulator(server : ActorRef, params : NetworkSimulatorParameters) extends Actor {
  /**
    * A message for handling the input queue during a scheduled task.
    */
  private final case class ProcessInputQueue()

  /**
    * A message for handling the output queue during a scheduled task.
    */
  private final case class ProcessOutputQueue()

  private val log = org.log4s.getLogger

  import scala.concurrent.ExecutionContext.Implicits.global
  val packetDelayDuration = (params.packetDelay/2).milliseconds

  type QueueItem = (Udp.Message, Long)
  implicit val QueueItem = Ordering.by[QueueItem, Long](_._2).reverse //sort in ascending order (older things get dequeued first)
  val inPacketQueue = mutable.PriorityQueue[QueueItem]()
  val outPacketQueue = mutable.PriorityQueue[QueueItem]()

  val chaos = new Random()
  var interface = ActorRef.noSender

  /**
    * The initial behavior demonstrated by this `Actor`.
    * Input and output indications are met with a timed decision to send the received message onwards.
    * While the deciding factor is different depending on what sort of message queue will be used,
    * the actual behavior is generally the same in certain ways.
    * @return a partial function
    */
  def receive = {
    case ProcessInputQueue() =>
      val time = System.nanoTime()
      var exit = false
      while(inPacketQueue.nonEmpty && !exit) {
        val lastTime = time - inPacketQueue.head._2
        if(lastTime >= 20000000) { //this packet needs to be sent within 20ms or more
          server.tell(inPacketQueue.dequeue._1, interface)
        }
        else {
          schedule(lastTime.nanoseconds, outbound = false)
          exit = true
        }
      }

    case ProcessOutputQueue() =>
      val time = System.nanoTime()
      var exit = false
      while(outPacketQueue.nonEmpty && !exit) {
        val lastTime = time - outPacketQueue.head._2
        if(lastTime >= 20000000) { //this packet needs to be sent within 20ms or more
          interface.tell(outPacketQueue.dequeue._1, server)
        }
        else {
          schedule(lastTime.nanoseconds, outbound = true)
          exit = true
        }
      }

    case msg @ Udp.Send(payload, target, _) => //outbound messages
      handlePacket(msg, outPacketQueue, outbound = true)

    case msg @ Udp.Received(payload, sender) => //inbound messages
      handlePacket(msg, inPacketQueue, outbound = false)

    case msg @ Udp.Bound(address) =>
      interface = sender()
      log.info(s"Hooked ${server.path} for network simulation")
      server.tell(msg, self) //make sure the server sends *us* the packets

    case default =>
      val from = sender()
      if(from == server)
        interface.tell(default, server)
      else if(from == interface)
        server.tell(default, interface)
      else
        log.error("Unexpected sending Actor " + from.path)
  }

  /**
    * A message has been received and is to be treated in a special way.
    * First, determine whether a task has been scheduled for processing.
    * (A message task is scheduled if absent.)
    * Then, the message is either shuffled in its queue or dispatched.
    * @param message the handled message
    * @param queue it doesn't do anything in this function
    * @param outbound which direction the message is going to be sent, indicating where the message originates
    */
  def handlePacket(message : Udp.Message, queue : mutable.PriorityQueue[QueueItem], outbound : Boolean) = {
    val name : String = if(outbound) "OUT" else "IN"
    val queue : mutable.PriorityQueue[QueueItem] = if(outbound) outPacketQueue else inPacketQueue

    if(chaos.nextDouble() > params.packetLoss) {
      if(queue.isEmpty) { //if the message queue is empty, then we need to reschedule our task
        schedule(packetDelayDuration, outbound)
      }
      if(chaos.nextDouble() <= params.packetReorderingChance) { //perform a reordering
        // creates the range (-1.0, 1.0)
        // time adjustment to move the packet (forward or backwards in time)
        val adj = (2*(chaos.nextDouble()-0.5)*params.packetReorderingTime).toLong
        queue += ((message, System.nanoTime() + adj*1000000))
        log.debug(s"Reordered $name by ${adj}ms - $message")
      }
      else { // normal message
        queue += ((message, System.nanoTime()))
      }
    }
    else {
      log.debug(s"Dropped $name - $message")
    }
  }

  /**
    * Prepare to send a message in the future.
    * @param duration how long until the message will be sent
    * @param outbound which direction the message is going to be sent, indicating where the message originates
    */
  def schedule(duration : FiniteDuration, outbound : Boolean) =
    context.system.scheduler.scheduleOnce(
      packetDelayDuration,
      self,
      if(outbound) ProcessOutputQueue() else ProcessInputQueue()
  )
}
