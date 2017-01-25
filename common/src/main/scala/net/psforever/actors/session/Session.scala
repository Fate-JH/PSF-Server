// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.session

import java.net.InetSocketAddress

import akka.actor.{ActorContext, ActorRef, MDCContextAware, PoisonPill}
import com.github.nscala_time.time.Imports._
import net.psforever.actors.udp.{HelloFriend, SendPacket}
import scodec.bits.ByteVector
import MDCContextAware.Implicits._

/**
  * A `Session` is the vehicle of packets being pushed upstream towards parent `Actors` and downstream towards child `Actors`.
  * Along the way it collects statistics about this communication.
  * @param sessionId an identifier for the associated session
  * @param socketAddress how this socket will be identified
  * @param returnActor outbound message destination
  * @param sessionPipeline inbound message destination
  * @param context a reference to the current part of the `ActorSystem`
  * @param self introspective reference?
  */
class Session(val sessionId : Long,
              val socketAddress : InetSocketAddress,
              returnActor : ActorRef,
              sessionPipeline : List[SessionPipeline])
             (implicit val context: ActorContext, implicit val self : ActorRef)  {

  var state : SessionState = New()
  val sessionCreatedTime : DateTime = DateTime.now
  var sessionEndedTime : DateTime = DateTime.now
  //statistics, out
  var bytesSent : Long = 0
  var outboundPackets : Long = 0
  var lastOutboundEvent : Long = System.nanoTime()
  var outboundPacketRate : Double = 0.0d
  var outboundBytesPerSecond : Double = 0.0d
  //statistics, in
  var bytesReceived : Long = 0
  var inboundPackets : Long = 0
  var lastInboundEvent : Long = System.nanoTime()
  var inboundPacketRate : Double = 0.0d
  var inboundBytesPerSecond : Double = 0.0d

  val pipeline = sessionPipeline.map { actor =>
    val a = context.actorOf(actor.props, actor.nameTemplate + sessionId.toString)
    context.watch(a)
    a
  }

  pipeline.head ! HelloFriend(sessionId, pipeline.tail.head) //be friendly; indoctrinate the children

  /**
    * Propagate downstream while collecting statistics.
    * @param packet the message (packet)
    */
  def receive(packet : RawPacket) : Unit = {
    bytesReceived += packet.data.size
    inboundPackets += 1
    lastInboundEvent = System.nanoTime()
    pipeline.head !> packet
  }

  /**
    * Propagate upstream while collecting statistics.
    * @param packet the message (packet)
    */
  def send(packet : ByteVector) : Unit = {
    bytesSent += packet.size
    outboundPackets += 1
    lastOutboundEvent = System.nanoTime()
    returnActor ! SendPacket(packet, socketAddress)
  }

  /**
    * Tell the downstream that we are terminating.
    * @param graceful we're always graceful
    */
  def dropSession(graceful : Boolean) = {
    pipeline.foreach(context.unwatch)
    pipeline.foreach(_ ! PoisonPill)
    sessionEndedTime = DateTime.now
    setState(Closed())
  }

  /**
    * Our current level of operation.
    * @return the state
    * @see `SessionState`
    */
  def getState = state

  /**
    * Change our current level of operation.
    * @param newState the state
    * @see `SessionState`
    */
  def setState(newState : SessionState) : Unit = {
    state = newState
  }

  /**
    * Get a reference to our downstream.
    * @return a `List` of `Actor`s this `Session` cares about
    */
  def getPipeline : List[ActorRef] = pipeline

  /**
    * Get the total communication that has been handled by this `Session`.
    * @return tx + rx, in bytes
    */
  def getTotalBytes = {
    bytesSent + bytesReceived
  }

  /**
    * When was the last time we received anything?
    * @return the time, in milliseconds, since the last inbound packet
    */
  def timeSinceLastInboundEvent = {
    (System.nanoTime() - lastInboundEvent)/1000000
  }

  /**
    * When was the last time we sent anything?
    * @return the time, in milliseconds, since the last outbound packet
    */
  def timeSinceLastOutboundEvent = {
    (System.nanoTime() - lastOutboundEvent)/1000000
  }

  /**
    * Mandatory override of `toString`.
    * @return the `sessionId` and measure of handled traffic
    */
  override def toString : String = {
    s"Session($sessionId, $getTotalBytes)"
  }
}
