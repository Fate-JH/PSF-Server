// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.mdc

import org.slf4j.MDC

import scala.concurrent.ExecutionContext

/**
  * A series of created anonymous classes that ultimately results in an `ExecutionContext`.
  * The ultimate executor loads a `Runnable` under a specific `MDC` signature and reports its thrown `Exception`s.
  * @see http://code.hootsuite.com/logging-contextual-info-in-an-asynchronous-scala-application/
  */
trait MDCPropagatingExecutionContext extends ExecutionContext {
  self => //name the self-type "self" so we can refer to it inside the nested class

  /**
    * Create an `ExecutionContext` for the purpose of uniquely instancing a `Runnable`.
    * @return an `ExecutionContext` that serves our needs
    * @see `java.lang.Thread`
    */
  override def prepare() : ExecutionContext = new ExecutionContext {
    val context : java.util.Map[String, String] = MDC.getCopyOfContextMap //Save the call-site MDC state

    /**
      * What happens when this context is executed.
      * @param r our work that needs to run
      * @see `java.lang.Runnable`
      */
    def execute(r: Runnable) : Unit = self.execute(new Runnable {
      /**
        * Manage the `MDC` context and then start up the `Runnable`.
        */
      def run() : Unit = {
        val oldContext = MDC.getCopyOfContextMap //Save the existing execution-site MDC state
        try {
          MDCPropagatingExecutionContext.setMDCContext(context) //set the call-site MDC state into the execution-site MDC
          r.run()
        }
        finally {
          MDCPropagatingExecutionContext.setMDCContext(oldContext) //restore the existing execution-site MDC state
        }
      }
    })

    /**
      * If we have failed, report what went wrong.
      * @param t an `Exception` or `Error` that we encountered
      * @see `java.lang.Throwable`
      */
    def reportFailure(t: Throwable) : Unit =
      self.reportFailure(t)
  }
}

object MDCPropagatingExecutionContext {
  object Implicits {
    /*
    Convenience wrapper around the Scala global ExecutionContext so you can just do:
    import MDCPropagatingExecutionContext.Implicits.global
     */
    implicit lazy val global = MDCPropagatingExecutionContextWrapper(ExecutionContext.Implicits.global)
  }

  def setMDCContext(context : java.util.Map[String, String]) : Unit = akka.actor.MDCContextAware.setMDCContext(context)
}
