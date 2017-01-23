// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.udp

import akka.actor.ActorRef

final case class HelloFriend(sessionId : Long, next: ActorRef)
