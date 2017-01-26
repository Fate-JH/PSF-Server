// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors

import java.net.InetSocketAddress

import akka.actor.MDCContextAware.Implicits._
import akka.actor.{Actor, ActorRef, Cancellable, MDCContextAware}
import net.psforever.actors.session.{DropSession, RawPacket}
import net.psforever.actors.udp.HelloFriend
import net.psforever.packet.control._
import net.psforever.packet.game._
import net.psforever.packet.{PlanetSideGamePacket, _}
import org.log4s.MDC
import scodec.Attempt.{Failure, Successful}
import scodec.bits._

import scala.concurrent.duration._

/**
  * An `Actor` that handles the process of logging into a game server and selecting a world to join.
  * It acts as a relay for more complicated packets involving synchronization and gameplay.
  */
class LoginSessionActor extends Actor with MDCContextAware {
  private[this] val log = org.log4s.getLogger("LoginSessionActor")

  var sessionId : Long = 0
  var leftRef : ActorRef = ActorRef.noSender
  var rightRef : ActorRef = ActorRef.noSender

  import scala.concurrent.ExecutionContext.Implicits.global
  private case class UpdateServerList()
  var updateServerListTask : Cancellable = null

  //TODO: move to global configuration or database lookup
  val serverName = "PSForever"
  val serverAddress = new InetSocketAddress(LoginConfig.serverIpAddress.getHostAddress, 51001)

  /**
    * The initial behavior demonstrated by this `Actor`.
    * Pass to the method `Initializing`.
    * @return a partial function
    */
  def receive = Initializing

  /**
    * The initial behavior demonstrated by this `Actor`.
    * Upon being greeted, the `Session` begins handling packets that are passed in between parent and child `Actors.`
    * @return a partial function
    * @see `Started`
    */
  def Initializing : Receive = {
    case HelloFriend(sessionId, right) =>
      this.sessionId = sessionId
      leftRef = sender()
      rightRef = right.asInstanceOf[ActorRef]
      context.become(Started)

    case _ =>
      log.error("Unknown message")
      context.stop(self)
  }

  /**
    * After the initial greeting, this behavior is moved to the top of the stack.
    * Subsequent activity is handled by the guidelines of this behavior.
    * Primarily, it is a packet routing routine.
    * @return a partial function
    */
  def Started : Receive = {
    case UpdateServerList() =>
      updateServerList()

    case ctrl @ ControlPacket(_, _) =>
      handlePktContainer(ctrl)

    case game @ GamePacket(_, _, _) =>
      handlePktContainer(game)

    case default =>
      failWithError(s"Invalid packet class received: $default")
  }

  /**
    * Deal with a normal PlanetSide packet.
    * @param pkt the packet
    */
  def handlePkt(pkt : PlanetSidePacket) : Unit = pkt match {
    case ctrl : PlanetSideControlPacket =>
      handleControlPkt(ctrl)

    case game : PlanetSideGamePacket =>
      handleGamePkt(game)

    case default =>
      failWithError(s"Invalid packet class received: $default")
  }

  /**
    * Deal with a nested packet.
    * In practice, we merely need to disentangle the actual packet from its wrapper and handle it as expected its type.
    * @param pkt the packet
    */
  def handlePktContainer(pkt : PlanetSidePacketContainer) : Unit = pkt match {
    case ctrl @ ControlPacket(opcode, ctrlPkt) =>
      handleControlPkt(ctrlPkt)

    case game @ GamePacket(opcode, seq, gamePkt) =>
      handleGamePkt(gamePkt)

    case default =>
      failWithError(s"Invalid packet container class received: $default")
  }

  /**
    * Deal with a control packet.
    * Two types of control packets are merely containers for multiple packets and they must be unraveled.
    * Each packet resulting from that unravelling can be handled as per that packet's type.
    * The third type of control packet assists in maintaining the connection between the client and server.
    * @param pkt the packet
    */
  def handleControlPkt(pkt : PlanetSideControlPacket) = {
    pkt match {
      case SlottedMetaPacket(slot, subslot, innerPacket) =>
        sendResponse(PacketCoding.CreateControlPacket(SlottedMetaAck(slot, subslot)))
        PacketCoding.DecodePacket(innerPacket) match {
          case Failure(e) =>
            log.error(s"Failed to decode inner packet of SlottedMetaPacket: $e")

          case Successful(v) =>
            handlePkt(v)
        }

      //IMPORTANT: ControlSync packets must be handled before MultiPackets are handled
      case sync @ ControlSync(diff, unk, f1, f2, f3, f4, fa, fb) =>
        log.trace(s"SYNC: ${sync}")
        val serverTick = Math.abs(System.nanoTime().toInt) // limit the size to prevent encoding error
        sendResponse(PacketCoding.CreateControlPacket(ControlSyncResp(diff, serverTick, fa, fb, fb, fa)))

      case MultiPacket(packets) =>
        packets.foreach { pkt =>
          PacketCoding.DecodePacket(pkt) match {
            case Failure(e) =>
              log.error(s"Failed to decode inner packet of MultiPacket: $e")

            case Successful(v) =>
              handlePkt(v)
          }
        }

      case default =>
        log.debug(s"Unhandled ControlPacket $default")
    }
  }

  /**
    * Deal with a normal PlanetSide game packet.
    * Either show an initial list of servers/worlds and actually connecting to one of those servers/worlds.
    * No other formal game packets should be encountered at this point.
    * @param pkt the packet
    */
  def handleGamePkt(pkt : PlanetSideGamePacket) = pkt match {
      case LoginMessage(majorVersion, minorVersion, buildDate, username, password, token, revision) =>
        val clientVersion = s"Client Version: ${majorVersion}.${minorVersion}.${revision}, ${buildDate}"
        if(token.isDefined) {
          log.info(s"New login UN:$username Token:${token.get}. ${clientVersion}")
        }
        else {
          log.info(s"New login UN:$username PW:$password. ${clientVersion}")
        }
        val newToken = token.getOrElse("THISISMYTOKENYES")
        //TODO hardcode LoginRespMessage, if it must look like this
        val response = LoginRespMessage(newToken, hex"00000000 18FABE0C 00000000 00000000", 0, 1, 2, 685276011, username, 10001)
        sendResponse(PacketCoding.CreateGamePacket(0, response))
        updateServerListTask = context.system.scheduler.schedule(0 seconds, 2 seconds, self, UpdateServerList())

      case ConnectToWorldRequestMessage(name, _, _, _, _, _, _) =>
        log.info(s"Connect to world request for '${name}'")
        val response = ConnectToWorldMessage(serverName, serverAddress.getHostString, serverAddress.getPort)
        sendResponse(PacketCoding.CreateGamePacket(0, response))
        sendResponse(DropSession(sessionId, "user transferring to world"))
        //TODO should we stop refreshing the server list manually?

      case default =>
        log.debug(s"Unhandled GamePacket ${pkt}")
  }

  /**
    * Dispatch a packet that tells the client about the available servers.
    * The non-technical term for this process is an "announce."
    */
  def updateServerList() = {
    //TODO when the login server and the game server separate, information about servers is provided by external conenctions?
    val msg = VNLWorldStatusMessage("Welcome to PlanetSide! ",
      Vector(
        WorldInformation(
          serverName,
          WorldStatus.Up,
          ServerType.Released,
          Vector(WorldConnectionInfo(serverAddress)),
          PlanetSideEmpire.VS //cockroaches
        )
      ))
    sendResponse(PacketCoding.CreateGamePacket(0, msg))
  }

  /**
    * What happens when this `Actor` stops.
    * Stop updating the server list.
    */
  override def postStop() = {
    if(updateServerListTask != null)
      updateServerListTask.cancel()
  }

  /**
    * Log the problem that was encountered.
    * @param error description of the error
    */
  def failWithError(error : String) = {
    log.error(error)
    //sendResponse(PacketCoding.CreateControlPacket(ConnectionClose()))
  }

  /**
    * Send a message towards the outside network.
    * @param cont the message or packet
    */
  def sendResponse(cont : Any) = {
    log.trace("LOGIN SEND: " + cont)
    MDC("sessionId") = sessionId.toString
    rightRef !> cont
  }

  /**
    * Send a message towards the outside network.
    * Guaranteed data in byte form, it is repackaged into a `RawPacket` before continuing on its journey.
    * @param pkt the message
    */
  def sendRawResponse(pkt : ByteVector) = {
    log.trace("LOGIN SEND RAW: " + pkt)
    MDC("sessionId") = sessionId.toString
    rightRef !> RawPacket(pkt)
  }
}
