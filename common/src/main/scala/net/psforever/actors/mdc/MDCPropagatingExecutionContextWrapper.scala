// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.mdc

import scala.concurrent.ExecutionContext

/**
  * Wrapper around an existing ExecutionContext that makes it propagate MDC information.
  */
class MDCPropagatingExecutionContextWrapper(wrapped: ExecutionContext)
  extends ExecutionContext with MDCPropagatingExecutionContext {

  override def execute(r: Runnable): Unit = wrapped.execute(r)

  override def reportFailure(t: Throwable): Unit = wrapped.reportFailure(t)
}

object MDCPropagatingExecutionContextWrapper {
  def apply(wrapped: ExecutionContext): MDCPropagatingExecutionContextWrapper = {
    new MDCPropagatingExecutionContextWrapper(wrapped)
  }
}
