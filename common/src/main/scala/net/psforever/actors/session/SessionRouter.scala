// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.session

import java.net.InetSocketAddress

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{Actor, ActorRef, MDCContextAware, OneForOneStrategy, Terminated}
import net.psforever.actors.udp.{Hello, ReceivedPacket}
import net.psforever.packet.PacketCoding
import net.psforever.packet.control.ConnectionClose
import org.log4s.MDC
import scala.concurrent.duration._

import scala.collection.mutable

/**
  * Transfer messages (as packets) between specific `Actor` `Session`s, matched to their identifiers.
  * Establish a workflow proceeding from one `Actor` to the next.
  * @param role a descriptor for this router's purpose
  * @param pipeline a `List` of `Actors` to be created and to receive and send messages
  */
class SessionRouter(role : String, pipeline : List[SessionPipeline]) extends Actor with MDCContextAware {
  private[this] val log = org.log4s.getLogger(self.path.name)

  val idBySocket = mutable.Map[InetSocketAddress, Long]()
  val sessionById = mutable.Map[Long, Session]()
  val sessionByActor = mutable.Map[ActorRef, Session]()
  val closePacket = PacketCoding.EncodePacket(ConnectionClose()).require.bytes
  var sessionId = 0L //this is a connection session, not an actual logged-in session ID
  var inputRef : ActorRef = ActorRef.noSender

  import scala.concurrent.ExecutionContext.Implicits.global
  val sessionReaper = context.system.scheduler.schedule(10 seconds, 5 seconds, self, SessionReaper()) //scheduled "task"

  /**
    * In our one-for-one strategy, all faults lead to that child `Stop`ping.
    * @return the strategy
    */
  override def supervisorStrategy = OneForOneStrategy() {
    case _ => Stop
  }

  /**
    * What happens when the `Actor` starts.
    */
  override def preStart = {
    log.info(s"SessionRouter started...ready for ${role} sessions")
  }

  /**
    * The initial behavior demonstrated by this `Actor`.
    * Pass to the method `initialize`.
    * @return a partial function
    */
  def receive = initializing

  /**
    * The initial behavior demonstrated by this `Actor`.
    * Upon being greeted, the `Session` begins handling packets that are passed in between parent and child `Actors.`
    * @return a partial function
    * @see `started`
    */
  def initializing : Receive = {
    case Hello() =>
      inputRef = sender()
      context.become(started)

    case default =>
      log.error(s"Unknown message $default. Stopping...")
      context.stop(self)
  }

  /**
    * After the initial function has been successfully progressed, this behavior is moved to the top of the stack.
    * Subsequent activity is handled by the guidelines of this behavior.
    * Received packets are sent along to their destination, whether downstream or upstream.
    * Both directions require some method of determining either the `Session` of origin or the `Session` of destination.
    * Other cases deal with `Sessions` that are no longer necessary to maintain.
    * @return a partial function
    */
  def started : Receive = {
    case recv @ ReceivedPacket(msg, from) =>
      var session : Session = null
      if(!idBySocket.contains(from)) {
        session = createNewSession(from)
      }
      else {
        val id = idBySocket{from}
        session = sessionById{id}
      }
      if(session.state != Closed()) {
        MDC("sessionId") = session.sessionId.toString
        log.trace(s"RECV: ${msg} -> ${session.getPipeline.head.path.name}")
        session.receive(RawPacket(msg))
        MDC.clear()
      }

    case ResponsePacket(msg) =>
      val session = sessionByActor.get(sender())
      if(session.isDefined) {
        if(session.get.state != Closed()) {
          MDC("sessionId") = session.get.sessionId.toString
          log.trace(s"SEND: ${msg} -> ${inputRef.path.name}")
          session.get.send(msg)
          MDC.clear()
        }
      }
      else {
        log.error("Dropped old response packet from actor " + sender().path.name)
      }

    case DropSession(id, reason) =>
      val session = sessionById.get(id)
      if(session.isDefined) {
        removeSessionById(id, reason, graceful = true)
      }
      else {
        log.error(s"Requested to drop non-existent session ID=$id from ${sender()}")
      }

    case SessionReaper() =>
      sessionById.foreach { case (id, session) =>
        log.debug(session.toString)
        if(session.getState == Closed()) {
          //clear mappings
          session.getPipeline.foreach(sessionByActor remove)
          sessionById.remove(id)
          idBySocket.remove(session.socketAddress)
          log.debug(s"Reaped session ID=$id")
        }
        else if(session.timeSinceLastInboundEvent > 10000) {
          removeSessionById(id, "session timed out (inbound)", graceful = false)
        }
        else if(session.timeSinceLastOutboundEvent > 4000) {
          removeSessionById(id, "session timed out (outbound)", graceful = true)
        }
      }

    case Terminated(actor) =>
      val terminatedSession = sessionByActor.get(actor)
      if(terminatedSession.isDefined) {
        removeSessionById(terminatedSession.get.sessionId, s"${actor.path.name} died", graceful = true)
      }
      else {
        log.error("Received an invalid actor Termination from " + actor.path.name)
      }

    case default =>
      log.error(s"Unknown message $default from " + sender().path)
  }

  /**
    * What happens when this `Actor` stops.
    * We no longer need to manage children.
    */
  override def postStop() = {
    sessionReaper.cancel()
  }

  /**
    * Start a new `Session` to be tracked.
    * @param address the internet protocol address attached to the new `Session`
    * @return the newly-created `Session`
    */
  private def createNewSession(address : InetSocketAddress) = {
    val id = newSessionId
    val session = new Session(id, address, inputRef, pipeline)
    //establish mappings for easy lookup
    idBySocket{address} = id
    sessionById{id} = session
    session.getPipeline.foreach { actor =>
      sessionByActor{actor} = session
    }
    log.info(s"New session ID=${id} from " + address.toString)
    session
  }

  /**
    * Stop tracking a `Session`.
    * @param id an identifier for the associated session
    * @param reason an explanation why this is happening
    * @param graceful if `true`, formal and careful efforts are taken that our related `Actors` know to terminate
    */
  private def removeSessionById(id : Long, reason : String, graceful : Boolean) : Unit = {
    val sessionOption = sessionById.get(id)
    if(sessionOption.nonEmpty) {
      val session : Session = sessionOption.get
      if(graceful) {
        for(i <- 0 to 5) {
          session.send(closePacket)
        }
      }
      session.dropSession(graceful) //kill all session specific actors
      log.info(s"Dropping session ID=${id} (reason: $reason)")
    }
  }

  /**
    * Get a new session identifier number.
    * Each id is just incremental of the previous one.
    * In the case of an invalid value we begin from `0L` again.
    * @return the identifier to use
    */
  private def newSessionId = {
    val oldId = sessionId
    if(sessionId < 0 || sessionId == Long.MaxValue) {
      log.info("Next session id would be invalid.  Resetting ids.")
      sessionId = 0L
    }
    else {
      sessionId += 1
    }
    oldId
  }
}
