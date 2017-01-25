// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.udp

/**
  * A more formal greeting between `Actors` created by the `UdpListener`.
  * It provides information that will assist the next.
  * @param sessionId an identifier for the associated session
  * @param next an `Actor` to which we send messages
  * @see Hello
  */
final case class HelloFriend(sessionId : Long, next: akka.actor.ActorRef)
