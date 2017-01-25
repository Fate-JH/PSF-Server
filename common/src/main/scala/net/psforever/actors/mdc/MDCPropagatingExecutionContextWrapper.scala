// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.mdc

import scala.concurrent.ExecutionContext

/**
  * Wrapper around an existing `ExecutionContext` for simplicity.
  * @param wrapped the context
  * @see `MDCPropagatingExecutionContext` (object)
  */
class MDCPropagatingExecutionContextWrapper(wrapped: ExecutionContext)
  extends ExecutionContext with MDCPropagatingExecutionContext {
  /**
    * Override to pass the parameter off to the internal context.
    * @param r our work that needs to run
    * @see `java.lang.Runnable`
    */
  override def execute(r: Runnable): Unit = wrapped.execute(r)

  /**
    * Override to pass the parameter off to the internal context.
    * @param t an `Exception` or `Error` that we encountered
    * @see `java.lang.Throwable`
    */
  override def reportFailure(t: Throwable): Unit = wrapped.reportFailure(t)
}

object MDCPropagatingExecutionContextWrapper {
  /**
    * Parallel constructor for an `MDCPropagatingExecutionContextWrapper`, to avoid explicit use of the `new` operator.
    * @param wrapped the context
    * @return an `MDCPropagatingExecutionContextWrapper`
    */
  def apply(wrapped: ExecutionContext): MDCPropagatingExecutionContextWrapper = {
    new MDCPropagatingExecutionContextWrapper(wrapped)
  }
}
