// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.udp

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{Actor, ActorRef, OneForOneStrategy, Props, Terminated}
import akka.io.{IO, Udp}
import scodec.interop.akka._

/**
  * An `Actor` that manages a loose UDP (user datagram protocol) connection between the application and a port.
  * When a connection is bound, the specified `Actor` is created as the recipient of incoming communications.
  * We are the medium between it and the ports.
  * Upon death of this child `Actor`, this listener creates a new one to take its place.
  * @param nextActorProps the next `Actor` to be created
  * @param nextActorName the name of the next `Actor`
  * @param listenAddress the internet protocol address through which messages will move
  * @param port identified by this port number
  * @param netParams information to contruct and test a simulation of a network;
  *                  `None` under normal operating procedures
  * @see `akka.io.Udp`
  */
class UdpListener(nextActorProps : Props,
                  nextActorName : String,
                  listenAddress : InetAddress,
                  port : Int,
                  netParams : Option[NetworkSimulatorParameters] = None) extends Actor {
  private val log = org.log4s.getLogger(self.path.name)
  var bytesRecevied = 0L
  var bytesSent = 0L
  var nextActor : ActorRef = Actor.noSender

  /**
    * In our one-for-one strategy, all faults lead to that child `Stop`ping.
    * @return the strategy
    */
  override def supervisorStrategy = OneForOneStrategy() {
    case _ => Stop
  }

  import context.system

  /*
  If we have network parameters, start the network simulator.
  We can't do `Props(new Actor)` here.
  See: http://www.cakesolutions.net/teamblogs/understanding-akkas-recommended-practice-for-actor-creation-in-scala.
   */
  if(netParams.isDefined) {
    val sim = context.actorOf(Props(classOf[UdpNetworkSimulator], self, netParams.get)) //sneaky and roundabout
    IO(Udp).tell(Udp.Bind(sim, new InetSocketAddress(listenAddress, port)), sim)
  }
  else {
    IO(Udp) ! Udp.Bind(self, new InetSocketAddress(listenAddress, port))
  }

  /**
    * The initial behavior demonstrated by this `Actor`.
    * If we bind (in as far as `Udp` permits), our child `Actor` is instantiated.
    * @return a partial function
    * @see `createNextActor`
    */
  def receive = {
    case Udp.Bound(local) =>
      log.info(s"Now listening on UDP:$local")
      createNextActor()
      context.become(ready(sender()))

    case Udp.CommandFailed(Udp.Bind(_, address, _)) =>
      log.error("Failed to bind to the network interface: " + address)
      context.system.terminate()

    case default =>
      log.error(s"Unexpected message $default")
  }

  /**
    * After the initial function has been successfully progressed, this behavior is moved to the top of the stack.
    * Subsequent activity is handled by the guidelines of this behavior.
    * Messages are just passed along whichever direction they are headed.
    * @param socket the originator of this activity
    * @return a partial function
    */
  def ready(socket : ActorRef) : Receive = {
    case SendPacket(msg, to) => //'msg' is a ByteVector
      bytesSent += msg.size
      socket ! Udp.Send(msg.toByteString, to)

    case Udp.Received(data, remote) => //'data' is a ByteString
      bytesRecevied += data.size
      nextActor ! ReceivedPacket(data.toByteVector, remote)

    case Udp.Unbind  =>
      socket ! Udp.Unbind

    case Udp.Unbound =>
      context.stop(self)

    case Terminated(actor) =>
      log.error(s"Next actor ${actor.path.name} has died...restarting")
      createNextActor()

    case default =>
      log.error(s"Unhandled message: $default")
  }

  /**
    * Instantiate a child `Actor` that was specified when this class was created.
    * Greet it warmly. :)
    */
  def createNextActor() = {
    nextActor = context.actorOf(nextActorProps, nextActorName)
    context.watch(nextActor)
    nextActor ! Hello()
  }
}
