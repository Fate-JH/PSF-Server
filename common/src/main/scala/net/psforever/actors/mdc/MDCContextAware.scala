// Copyright (c) 2016 PSForever.net to present
package akka.actor
/*
Just to clarify, these classes are not located in the same package as the physical file.
It should be in package akka.actor
 */

import akka.util.Timeout
import org.slf4j.MDC

import scala.concurrent.Future

/**
  * Enhance the already existing `Trait` `MDCContextAware` with new functionality.
  * (That is why it needs to be in the `package` `akka.actor`.)<br>
  * <br>
  * The mapped diagnostic context is a system that assists the logging of events and errors by uniquely identifying the origin.
  * If the "classic" suggestion is a different error-logging routine for each client (or thread) acting in a system,
  * the `MDC` allows for a lighter solution of feeding different-sourced information into one logging routine.
  * In `Log4j`, this is also referred to as "Thread Context Map."
  */
trait MDCContextAware extends Actor with ActorLogging  {
  import MDCContextAware._

  /**
    * Overrode to change the `Actor`'s current message receiving behavior.
    * Two things happen:
    * the first is that the `MDC` gets reset;
    * the second is that this message or the original message is passed on to the superclass.
    * @param receive a partial function that describes what to do when a message is received
    * @param msg the message
    */
  override protected[akka] def aroundReceive(receive: Actor.Receive, msg: Any): Unit = {
    val orig = MDC.getCopyOfContextMap
    try {
      msg match {
        case mdcObj @ MdcMsg(mdc, origMsg) =>
          MDCContextAware.setMDCContext(mdc)
          super.aroundReceive(receive, origMsg)

        case _ =>
          super.aroundReceive(receive, msg)
      }
    }
    finally {
      MDCContextAware.setMDCContext(orig)
    }
  }
}

object MDCContextAware {
  private case class MdcMsg(mdc: java.util.Map[String, String], msg: Any)

  object Implicits {
    /**
      * Add two new methods that allow `MDC` info to be passed to `MDCContextAware` actors.
      * Do NOT use these methods to send to actors that are not `MDCContextAware`.
      */
    implicit class ContextLocalAwareActorRef(val ref: ActorRef) extends AnyVal {
      import akka.pattern.ask
      /**
        * Send a message to an `Actor` that is `MDCContextAware`.
        * It will propagate the current `MDC` values.
        * We MUST capture the `ActorContext` in order for senders to be correct!
        * This was a bug from the original author.
        * @param msg the message
        */
      def !>(msg: Any)(implicit context: ActorContext) : Unit =
        ref.tell(MdcMsg(MDC.getCopyOfContextMap, msg), context.self)

      /**
        * "Ask" an actor that is `MDCContextAware` for something.
        * It will propagate the current `MDC` values.
        * @param msg the message
        */
      def ?>(msg: Any)(implicit context: ActorContext, timeout: Timeout): Future[Any] =
        ref.ask(MdcMsg(MDC.getCopyOfContextMap, msg), context.self)
    }
  }

  /**
    * Change the `MDC` context.
    * @param context a new context, maybe `null`
    */
  def setMDCContext(context : java.util.Map[String, String]) : Unit = {
    if(context != null) {
      MDC.setContextMap(context)
    }
    else {
      MDC.clear()
    }
  }
}
