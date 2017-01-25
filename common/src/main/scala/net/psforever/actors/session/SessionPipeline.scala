// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.session

/**
  * Information about an ordered series of `Actors`.
  * @param nameTemplate a descriptor for this pipeline's purpose
  * @param props an `Actor` to be created and to receive and send messages
  */
case class SessionPipeline(nameTemplate : String, props : akka.actor.Props)
