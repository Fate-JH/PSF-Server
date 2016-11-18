// Copyright (c) 2016 PSForever.net to present
import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, Cancellable, MDCContextAware}
import net.psforever.packet.{PlanetSideGamePacket, _}
import net.psforever.packet.control._
import net.psforever.packet.game._
import scodec.Attempt.{Failure, Successful}
import scodec.bits._
import org.log4s.MDC
import MDCContextAware.Implicits._
import net.psforever.types.ChatMessageType
import scodec.codecs._

import scala.collection.mutable
import scala.collection.immutable
import scala.util.Random

class WorldSessionActor extends Actor with MDCContextAware {
  private[this] val log = org.log4s.getLogger

  private case class PokeClient()

  var sessionId : Long = 0
  var leftRef : ActorRef = ActorRef.noSender
  var rightRef : ActorRef = ActorRef.noSender

  var clientKeepAlive : Cancellable = null

  override def postStop() = {
    if(clientKeepAlive != null)
      clientKeepAlive.cancel()
  }

  def receive = Initializing

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

  def Started : Receive = {
    case ctrl @ ControlPacket(_, _) =>
      handlePktContainer(ctrl)
    case game @ GamePacket(_, _, _) =>
      handlePktContainer(game)
      // temporary hack to keep the client from disconnecting
    case PokeClient() =>
      sendResponse(PacketCoding.CreateGamePacket(0, KeepAliveMessage(0)))
    case default => failWithError(s"Invalid packet class received: $default")
  }

  def handlePkt(pkt : PlanetSidePacket) : Unit = pkt match {
    case ctrl : PlanetSideControlPacket =>
      handleControlPkt(ctrl)
    case game : PlanetSideGamePacket =>
      handleGamePkt(game)
    case default => failWithError(s"Invalid packet class received: $default")
  }

  def handlePktContainer(pkt : PlanetSidePacketContainer) : Unit = pkt match {
    case ctrl @ ControlPacket(opcode, ctrlPkt) =>
      handleControlPkt(ctrlPkt)
    case game @ GamePacket(opcode, seq, gamePkt) =>
      handleGamePkt(gamePkt)
    case default => failWithError(s"Invalid packet container class received: $default")
  }

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
      case sync @ ControlSync(diff, unk, f1, f2, f3, f4, fa, fb) =>
        log.debug(s"SYNC: ${sync}")
        val serverTick = Math.abs(System.nanoTime().toInt) // limit the size to prevent encoding error
        sendResponse(PacketCoding.CreateControlPacket(ControlSyncResp(diff, serverTick,
          fa, fb, fb, fa)))
      case MultiPacket(packets) =>
        packets.foreach { pkt =>
          PacketCoding.DecodePacket(pkt) match {
            case Failure(e) =>
              log.error(s"Failed to decode inner packet of MultiPacket: $e")
            case Successful(v) =>
              handlePkt(v)
          }
        }
      case MultiPacketEx(packets) =>
        packets.foreach { pkt =>
          PacketCoding.DecodePacket(pkt) match {
            case Failure(e) =>
              log.error(s"Failed to decode inner packet of MultiPacketEx: $e")
            case Successful(v) =>
              handlePkt(v)
          }
        }
      case default =>
        log.debug(s"Unhandled ControlPacket $default")
    }
  }

  // XXX: hard coded ObjectCreateMessage
  val objectHex = hex"18 57 0C 00 00 BC 84 B0  06 C2 D7 65 53 5C A1 60 00 01 34 40 00 09 70 49  00 6C 00 6C 00 6C 00 49 00 49 00 49 00 6C 00 6C  00 6C 00 49 00 6C 00 49 00 6C 00 6C 00 49 00 6C  00 6C 00 6C 00 49 00 6C 00 6C 00 49 00 84 52 70  76 1E 80 80 00 00 00 00 00 3F FF C0 00 00 00 20  00 00 0F F6 A7 03 FF FF FF FF FF FF FF FF FF FF  FF FF FF FF FF FD 90 00 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 00 01 90 01 90 00 64 00  00 01 00 7E C8 00 C8 00 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 00 00 01 C0 00 42 C5 46  86 C7 00 00 00 80 00 00 12 40 78 70 65 5F 73 61  6E 63 74 75 61 72 79 5F 68 65 6C 70 90 78 70 65  5F 74 68 5F 66 69 72 65 6D 6F 64 65 73 8B 75 73  65 64 5F 62 65 61 6D 65 72 85 6D 61 70 31 33 00  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 00 00 00 00 01 0A 23 02  60 04 04 40 00 00 10 00 06 02 08 14 D0 08 0C 80  00 02 00 02 6B 4E 00 82 88 00 00 02 00 00 C0 41  C0 9E 01 01 90 00 00 64 00 44 2A 00 10 91 00 00  00 40 00 18 08 38 94 40 20 32 00 00 00 80 19 05  48 02 17 20 00 00 08 00 70 29 80 43 64 00 00 32  00 0E 05 40 08 9C 80 00 06 40 01 C0 AA 01 19 90  00 00 C8 00 3A 15 80 28 72 00 00 19 00 04 0A B8  05 26 40 00 03 20 06 C2 58 00 A7 88 00 00 02 00  00 80 00 00 "
  val traveler = Traveler(this, "home3")

  def handleGamePkt(pkt : PlanetSideGamePacket) = pkt match {
    case ConnectToWorldRequestMessage(server, token, majorVersion, minorVersion, revision, buildDate, unk) =>

      val clientVersion = s"Client Version: ${majorVersion}.${minorVersion}.${revision}, ${buildDate}"

      log.info(s"New world login to ${server} with Token:${token}. ${clientVersion}")

      // ObjectCreateMessage
      sendRawResponse(objectHex)
      // XXX: hard coded message
      sendRawResponse(hex"14 0F 00 00 00 10 27 00  00 C1 D8 7A 02 4B 00 26 5C B0 80 00 ")

      // NOTE: PlanetSideZoneID just chooses the background
      sendResponse(PacketCoding.CreateGamePacket(0,
        CharacterInfoMessage(PlanetSideZoneID(1), 0, PlanetSideGUID(0), true, 0)))
    case msg @ CharacterRequestMessage(charId, action) =>
      log.info("Handling " + msg)

      action match {
        case CharacterRequestAction.Delete =>
          sendResponse(PacketCoding.CreateGamePacket(0, ActionResultMessage(false, Some(1))))
        case CharacterRequestAction.Select =>
          PacketCoding.DecodeGamePacket(objectHex).require match {
            case obj @ ObjectCreateMessage(len, cls, guid, _, _) =>
              log.debug("Object: " + obj)
              // LoadMapMessage 13714 in mossy .gcap
              // XXX: hardcoded shit
              sendResponse(PacketCoding.CreateGamePacket(0, ZonePopulationUpdateMessage(PlanetSideGUID(13), 414, 138, 0, 138, 0, 138, 0, 138, 0)))
              val home3 = Zone.get("home3").get
              Transfer.loadMap(traveler, home3)
              Transfer.loadSelf(traveler, Zone.default(home3))

              // These object_guids are specfic to VS Sanc
              sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(2), PlanetSideEmpire.VS))) //HART building C
              sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(29), PlanetSideEmpire.NC))) //South Villa Gun Tower

              sendResponse(PacketCoding.CreateGamePacket(0, TimeOfDayMessage(0, 4653056, 0, 0, 32, 65)))
              sendResponse(PacketCoding.CreateGamePacket(0, ContinentalLockUpdateMessage(PlanetSideGUID(13), PlanetSideEmpire.VS))) // "The VS have captured the VS Sanctuary."
              sendResponse(PacketCoding.CreateGamePacket(0, BroadcastWarpgateUpdateMessage(PlanetSideGUID(13), PlanetSideGUID(1), 32))) // VS Sanctuary: Inactive Warpgate -> Broadcast Warpgate

              sendResponse(PacketCoding.CreateGamePacket(0,BuildingInfoUpdateMessage(
                PlanetSideGUID(6),   //Ceryshen
                PlanetSideGUID(2),   //Anguta
                8,                   //80% NTU
                true,                //Base hacked
                PlanetSideEmpire.NC, //Base hacked by NC
                600000,              //10 minutes remaining for hack
                PlanetSideEmpire.VS, //Base owned by VS
                0,                   //!! Field != 0 will cause malformed packet. See class def.
                PlanetSideGeneratorState.Critical, //Generator critical
                true,                //Respawn tubes destroyed
                true,                //Force dome active
                16,                  //Tech plant lattice benefit
                0,
                0,                   //!! Field > 0 will cause malformed packet. See class def.
                0,
                false,
                8,                   //!! Field != 8 will cause malformed packet. See class def.
                true,                //Boosted spawn room pain field
                true)))              //Boosted generator room pain field

              import scala.concurrent.duration._
              import scala.concurrent.ExecutionContext.Implicits.global
              clientKeepAlive = context.system.scheduler.schedule(0 seconds, 500 milliseconds, self, PokeClient())
          }
        case default =>
          log.error("Unsupported " + default + " in " + msg)
      }
    case msg @ CharacterCreateRequestMessage(name, head, voice, gender, empire) =>
      log.info("Handling " + msg)

      sendResponse(PacketCoding.CreateGamePacket(0, ActionResultMessage(true, None)))
      sendResponse(PacketCoding.CreateGamePacket(0,
        CharacterInfoMessage(PlanetSideZoneID(0), 0, PlanetSideGUID(0), true, 0)))

    case KeepAliveMessage(code) =>
      sendResponse(PacketCoding.CreateGamePacket(0, KeepAliveMessage(0)))

    case msg @ PlayerStateMessageUpstream(avatar_guid, pos, vel, unk1, aim_pitch, unk2, seq_time, unk3, is_crouching, unk4, unk5, unk6, unk7, unk8) =>
      //log.info("PlayerState: " + msg)

    case msg @ ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents) =>
      // TODO: Prevents log spam, but should be handled correctly
      if (messagetype != ChatMessageType.CMT_TOGGLE_GM) {
        log.info("Chat: " + msg)
      }

      CSRZone.read(traveler, msg)
      CSRWarp.read(traveler, msg)

      // TODO: handle this appropriately
      if(messagetype == ChatMessageType.CMT_QUIT) {
        sendResponse(DropCryptoSession())
        sendResponse(DropSession(sessionId, "user quit"))
      }

      // TODO: Depending on messagetype, may need to prepend sender's name to contents with proper spacing
      // TODO: Just replays the packet straight back to sender; actually needs to be routed to recipients!
      sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents)))

    case msg @ ChangeFireModeMessage(item_guid, fire_mode) =>
      log.info("ChangeFireMode: " + msg)

    case msg @ ChangeFireStateMessage_Start(item_guid) =>
      log.info("ChangeFireState_Start: " + msg)

    case msg @ ChangeFireStateMessage_Stop(item_guid) =>
      log.info("ChangeFireState_Stop: " + msg)

    case msg @ EmoteMsg(avatar_guid, emote) =>
      log.info("Emote: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, EmoteMsg(avatar_guid, emote)))

    case msg @ DropItemMessage(item_guid) =>
      log.info("DropItem: " + msg)

    case msg @ ReloadMessage(item_guid, ammo_clip, unk1) =>
      log.info("Reload: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ReloadMessage(item_guid, 123, unk1)))

    case msg @ ObjectHeldMessage(avatar_guid, held_holsters, unk1) =>
      log.info("ObjectHeld: " + msg)

    case msg @ AvatarJumpMessage(state) =>
      //log.info("AvatarJump: " + msg)

    case msg @ RequestDestroyMessage(object_guid) =>
      log.info("RequestDestroy: " + msg)
      // TODO: Make sure this is the correct response in all cases
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(object_guid, 0)))

    case msg @ ObjectDeleteMessage(object_guid, unk1) =>
      log.info("ObjectDelete: " + msg)

    case msg @ MoveItemMessage(item_guid, avatar_guid_1, avatar_guid_2, dest, unk1) =>
      log.info("MoveItem: " + msg)

    case msg @ ChangeAmmoMessage(item_guid, unk1) =>
      log.info("ChangeAmmo: " + msg)

    case msg @ UseItemMessage(avatar_guid, unk1, object_guid, unk2, unk3, unk4, unk5, unk6, unk7, unk8, unk9) =>
      log.info("UseItem: " + msg)
      // TODO: Not all fields in the response are identical to source in real packet logs (but seems to be ok)
      // TODO: Not all incoming UseItemMessage's respond with another UseItemMessage (i.e. doors only send out GenericObjectStateMsg)
      sendResponse(PacketCoding.CreateGamePacket(0, UseItemMessage(avatar_guid, unk1, object_guid, unk2, unk3, unk4, unk5, unk6, unk7, unk8, unk9)))
      // TODO: This should only actually be sent to doors upon opening; may break non-door items upon use
      sendResponse(PacketCoding.CreateGamePacket(0, GenericObjectStateMsg(object_guid, 16)))

    case msg @ GenericObjectStateMsg(object_guid, unk1) =>
      log.info("GenericObjectState: " + msg)

    case msg @ ItemTransactionMessage(terminal_guid, transaction_type, item_page, item_name, unk1, item_guid) =>
      log.info("ItemTransaction: " + msg)

    case msg @ WeaponDelayFireMessage(seq_time, weapon_guid) =>
      log.info("WeaponDelayFire: " + msg)

    case msg @ WeaponFireMessage(seq_time, weapon_guid, projectile_guid, shot_origin, unk1, unk2, unk3, unk4, unk5, unk6, unk7) =>
      log.info("WeaponFire: " + msg)

    case msg @ HitMessage(seq_time, projectile_guid, unk1, hit_info, unk2, unk3, unk4) =>
      log.info("Hit: " + msg)

    case msg @ AvatarFirstTimeEventMessage(avatar_guid, object_guid, unk1, event_name) =>
      log.info("AvatarFirstTimeEvent: " + msg)

    case default => log.debug(s"Unhandled GamePacket ${pkt}")
  }

  def failWithError(error : String) = {
    log.error(error)
    //sendResponse(PacketCoding.CreateControlPacket(ConnectionClose()))
  }

  def sendResponse(cont : PlanetSidePacketContainer) : Unit = {
    log.trace("WORLD SEND: " + cont)
    sendResponse(cont.asInstanceOf[Any])
  }

  def sendResponse(msg : Any) : Unit = {
    MDC("sessionId") = sessionId.toString
    rightRef !> msg
  }

  def sendRawResponse(pkt : ByteVector) = {
    log.trace("WORLD SEND RAW: " + pkt)
    sendResponse(RawPacket(pkt))
  }

  def sendToSelf(msg : PlanetSidePacketContainer) : Unit = {
    sendResponse(msg)
  }

  def sendToSelf(msg : ByteVector) : Unit = {
    sendRawResponse(msg)
  }
}

// for development and fun; please remove this once we have continental transfer working
// at least, move it to a more presentable folder once we refactor WorldSessionActor
class Traveler(private val session : WorldSessionActor) {
  val player : ByteVector = session.objectHex
  var zone : String = ""

  def sendToSelf(msg : ByteVector) : Unit = {
    this.session.sendRawResponse(msg)
  }

  def sendToSelf(msg : PlanetSidePacketContainer) : Unit = {
    this.session.sendResponse(msg)
  }
}

object Traveler {
  def apply(session : WorldSessionActor) : Traveler = new Traveler(session)

  def apply(session : WorldSessionActor, zoneId : String) : Traveler = {
    val traveler = new Traveler(session)
    traveler.zone = zoneId
    traveler
  }
}

object CSRZone {
  def read(traveler : Traveler, msg : ChatMsg) : Boolean = {
    if(!isProperRequest(msg))
      return false
    val buffer = decomposeMessage(msg.contents)
    if(buffer.length == 0 || buffer(0).equals("-help")) {
      help(traveler)
      return false
    }

    var zoneId = ""
    var gateId = ""
    var displayWarpgateList = false
    for(o <- buffer) {
      if(o.equals("-list")) {
        if(zoneId.equals("")) {
          CSRZone.list(traveler, Zone.list)
          return false
        }
        else if(gateId.equals("")) {
          displayWarpgateList = true
        }
      }
      else if(zoneId.equals(""))
        zoneId = o
      else if(gateId.equals(""))
        gateId = o
    }

    val zoneOpt = Zone.get(zoneId)
    if(zoneOpt.isEmpty) {
      CSRZone.error(traveler, "/zone must be given a valid zonename (not"+zoneId+") (use '/zone -list')")
      return false
    }
    val zone = zoneOpt.get
    if(displayWarpgateList) {
      CSRZone.list(traveler, Zone.listWarpgates(zone))
      return false
    }
    traveler.zone = zoneId
    var destination : (Int, Int, Int) = Zone.default(zone)

    if(!gateId.equals("")) {
      val gateOpt = Zone.getWarpgate(zone, gateId)
      if(gateOpt.isDefined)
        destination = gateOpt.get
      else
        CSRZone.error(traveler, "gate id not defined for zone (use '/zone <zone> -list'")
    }
    Transfer.zone(traveler, zone, destination)
    true
  }

  def isProperRequest(msg : ChatMsg) : Boolean ={
    msg.messageType == ChatMessageType.CMT_ZONE
  }

  private def decomposeMessage(msg : String) : Array[String] = {
    msg.trim.toLowerCase.split("\\s+")
  }

  def help(traveler : Traveler) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", "use: /zone [<id> [warpgate] | [-list]", None))
    )
  }

  def list(traveler : Traveler, msg : String) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", msg, None))
    )
  }

  def error(traveler : Traveler, msg : String) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", "error: "+msg, None))
    )
  }
}

object CSRWarp {
  def read(traveler : Traveler, msg : ChatMsg) : Boolean = {
    if(!isProperRequest(msg))
      return false
    val buffer = decomposeMessage(msg.contents)
    if(buffer.length == 0 || buffer(0).equals("-help")) {
      help(traveler)
      return false
    }

    var destId = ""
    for(o <- buffer) {
      if(o.equals("-list")) {
        if(destId.equals("")) {
          CSRWarp.list(traveler, Zone.listLocations(Zone.get(traveler.zone).get))
          return false
        }
      }
      else if(destId.equals(""))
        destId = o
    }

    val zone = Zone.get(traveler.zone).get
    val dest = Zone.getWarpLocation(zone, destId)
    if(dest.isEmpty) {
      CSRWarp.error(traveler, "/warp must be given a valid location in this zone")
      return false
    }
    Transfer.warp(traveler, dest.get)
    true
  }

  def isProperRequest(msg : ChatMsg) : Boolean ={
    msg.messageType == ChatMessageType.CMT_WARP
  }

  private def decomposeMessage(msg : String) : Array[String] = {
    msg.trim.toLowerCase.split("\\s+")
  }

  def help(traveler : Traveler) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", "use: /warp [<id> | <x> <y> <z>] | [-list]", None))
    )
  }

  def list(traveler : Traveler, msg : String) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", msg, None))
    )
  }

  def error(traveler : Traveler, msg : String) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0,
      ChatMsg(ChatMessageType.CMT_OPEN,true,"", "error: "+msg, None))
    )
  }
}

object Transfer {
  def warp(traveler : Traveler, destination : (Int, Int, Int)): Unit = {
    disposeSelf(traveler)
    loadSelf(traveler, destination)
  }

  def zone(traveler : Traveler, zone : Zone, destination : (Int, Int, Int)) : Unit = {
    disposeSelf(traveler)
    loadMap(traveler, zone)
    loadSelf(traveler, destination)
  }

  private def disposeSelf(traveler : Traveler) : Unit = {
    //dispose inventory
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(76),4)))
    // 77?
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(78),4)))
    // 79?
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(80),4)))
    // 81-82?
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(83),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(84),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(85),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(86),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(87),4)))
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(88),4)))
    //dispose self
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(75),4)))
  }

  def loadMap(traveler : Traveler, zone : Zone) : Unit = {
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, LoadMapMessage(zone.map, zone.zonename, 40100,25,true,3770441820L)))
  }

  def loadSelf(traveler : Traveler, loc : (Int, Int, Int)) : Unit = {
    //calculate bit representation of modified coordinates
    val x : BitVector = uintL(12).encode(loc._1/2).toOption.get
    val y : BitVector = uintL(12).encode(loc._2/2).toOption.get
    val z : BitVector = uintL(12).encode(loc._3/4).toOption.get
    //dissect ObjectCreateMessage data portion
    val firstPart : BitVector = traveler.player.toBitVector.take(76) //first part
    var temp : BitVector = traveler.player.toBitVector.drop(88) //first part + 'x' were removed
    val secondPart : BitVector = temp.take(8) //first spacer
    temp = temp.drop(20) //first spacer and 'y' were removed
    val thirdPart : BitVector = temp.take(8) //second spacer
    //reconstitute ObjectCreateMessage data around new coordinates
    temp = firstPart ++ x ++ secondPart ++ y ++ thirdPart ++ z ++ temp.drop(20) //second spacer and 'z' were removed
    //send
    traveler.sendToSelf(temp.toByteVector)
    traveler.sendToSelf(PacketCoding.CreateGamePacket(0, SetCurrentAvatarMessage(PlanetSideGUID(75),0,0)))
  }
}

class Zone(val alias : String, val map : String, val zonename : String) {
  private val locations : mutable.HashMap[String, (Int, Int, Int)] = mutable.HashMap()
  private val gates : mutable.HashMap[String, (Int, Int, Int)] = mutable.HashMap()
}

object Zone {
  //the /zone command should use these names
  private val zones = immutable.HashMap[String, Zone](
    "z1" -> Zone("Solsar", "map01", "z1"),
    "z2" -> Zone("Hossin", "map02", "z2"),
    "z3" -> Zone("Cyssor", "map03", "z3"),
    "z4" -> Zone("Ishundar", "map04", "z4"),
    "z5" -> Zone("Forseral", "map05", "z5"),
    "z6" -> Zone("Ceryshen", "map06", "z6"),
    "z7" -> Zone("Esamir","map07", "z7"),
    "z8" -> Zone("Oshur", "map08", "z8"),
    "z9" -> Zone("Searhus","map09", "z9"),
    "z10" -> Zone("Amerish","map10", "z10"),
    "home1" -> Zone("NC Sanctuary", "map11", "home1"),
    "home2" -> Zone("TR Sanctuary", "map12", "home2"),
    "home3" -> Zone("VS Sanctuary", "map13", "home3"),
    "tzshtr" -> Zone("VR Shooting Range", "map14", "tzshtr"),
    "tzdrtr" -> Zone("VR Driving Range","map15", "tzdrtr"),
    "c1" -> Zone("Supai", "ugd01", "c1"),
    "c2" -> Zone("Hunhau", "ugd02", "c2"),
    "c3" -> Zone("Adlivun", "ugd03", "c3"),
    "c4" -> Zone("Byblos", "ugd04", "c4"),
    "c5" -> Zone("Annwn", "ugd05", "c5"),
    "c6" -> Zone("Drugaskan", "ugd06", "c6"),
    "i4" -> Zone("Nexus", "map96", "i4"),
    "i3" -> Zone("Desolation", "map97", "i3"),
    "i2" -> Zone("Ascension", "map98", "i2"),
    "i1" -> Zone("Extinction", "map99", "i1")
  )

  //for ease of use, these names can be applied too
  private val alias = immutable.HashMap[String, String](
    "solsar" -> "z1",
    "hossin" -> "z2",
    "cyssor" -> "z3",
    "ishundar" -> "z4",
    "forseral" -> "z5",
    "ceryshen" -> "z6",
    "esamir" -> "z7",
    "oshur" -> "z8",
    "searhus" -> "z9",
    "amerish" -> "z10",
    "nc-sanctuary" -> "home1",
    "tr-sanctuary" -> "home2",
    "vs-sanctuary" -> "home3",
    "shooting" -> "tzshtr",
    "driving" -> "tzdrtr",
    "supai" -> "c1",
    "hunhau" -> "c2",
    "adlivun" -> "c3",
    "byblos" -> "c4",
    "annwn" -> "c5",
    "drugaskan" -> "c6",
    "nexus" -> "i4",
    "desolation" -> "i3",
    "ascension" -> "i2",
    "extinction" -> "i1"
  )
  setup
  private val rand = Random

  def apply(name : String, map : String, title : String) : Zone = new Zone(name, map, title)

  def get(zoneId : String) : Option[Zone] = {
    var zId = zoneId.toLowerCase
    if(alias.get(zId).isDefined)
      zId = alias(zId)

    zones.get(zId)
  }

  def getWarpLocation(zone : Zone, wpoint : String) : Option[(Int, Int, Int)] = {
    val low_wpoint = wpoint.toLowerCase
    var location = zone.locations.get(low_wpoint)
    if(location.isEmpty)
      location = zone.gates.get(low_wpoint)
    location
  }

  def getWarpgate(zone : Zone, gate : String) : Option[(Int, Int, Int)] = {
    zone.gates.get(gate.toLowerCase)
  }

  def list : String = {
    "zonenames: z1 - z10, home1 - home3, tzshtr, tzdrtr, c1 - c6, i1 - i4; zones are also aliased their continent name"
  }

  def listLocations(zone : Zone) : String = {
    var out : String = "warps: "
    if(zone.locations.nonEmpty) {
      out += zone.locations.keys.toArray.sorted.mkString(", ")
    }
    else
      out = "none"
    out
  }

  def listWarpgates(zone : Zone) : String = {
    var out : String = "warpgates: "
    if(zone.gates.isEmpty)
      out += "none"
    else
      out += zone.gates.keys.toArray.sorted.mkString(", ")
    out
  }

  def default(zone : Zone) : (Int, Int, Int) = {
    var outlets = zone.locations //random location?
    if(outlets.nonEmpty) {
      return outlets.values.toArray.apply(rand.nextInt(outlets.size))
    }
    outlets = zone.gates //random warpgate?
    if(outlets.nonEmpty) {
      return outlets.values.toArray.apply(rand.nextInt(outlets.size))
    }
    (0, 0, 0) //fallback
  }

  //zones have locations of interest (bases, warpgates, the monolith, etc.)
  def setup() : Unit = {
    zones("z1").gates += (
      "gate1" -> (4150, 7341, 82),
      "gate2" -> (5698, 3404, 129),
      "gate3" -> (2650, 5363, 176),
      "gate4" -> (3022, 1225, 66),
      "geowarp1" -> (3678, 2895, 108),
      "geowarp2" -> (5672, 4750, 70)
      )
    zones("z1").locations += (
      "amun" -> (4337, 2278, 68),
      "aton" -> (3772, 5463, 54),
      "bastet" -> (5412, 5588, 56),
      "hapi" -> (4256, 4436, 59),
      "horus" -> (3725, 2114, 73),
      "mont" -> (3354, 4205, 83),
      "seth" -> (4495, 6026, 58),
      "sobek" -> (3094, 3027, 75),
      "thoth" -> (4615, 3373, 53),
      "lake" -> (4317, 4008, 37),
      "monolith" -> (5551, 5047, 64)
      )
    zones("z2").gates += (
      "gate1" -> (1881, 4873, 19),
      "gate2" -> (4648, 4625, 28),
      "gate3" -> (3296, 2045, 21),
      "gate4" -> (5614, 1781, 32),
      "geowarp1" -> (5199, 4869, 39),
      "geowarp2" -> (3911, 2407, 15)
      )
    zones("z2").locations += (
      "acan" -> (3534, 4015, 30),
      "bitol" -> (4525, 2632, 30),
      "chac" -> (4111, 5950, 39),
      "ghanon" -> (2565, 3707, 41),
      "hurakan" -> (1840, 2934, 38),
      "ixtab" -> (3478, 3143, 40),
      "kisin" -> (3356, 5374, 31),
      "mulac" -> (5592, 2738, 37),
      "naum" -> (5390, 3454, 28),
      "voltan" -> (4529, 3414, 28),
      "zotz" -> (6677, 2342, 129),
      "monolith" -> (2938, 2485, 14)
      )
    zones("z3").gates += (
      "gate1" -> (2616, 6567, 58),
      "gate2" -> (6980, 5336, 57),
      "gate3" -> (1199, 1332, 66),
      "gate4" -> (5815, 1974, 63),
      "geowarp1" -> (2403, 4278, 60),
      "geowarp2" -> (4722, 2665, 78)
      )
    zones("z3").locations += (
      "aja" -> (754, 5435, 48),
      "chuku" -> (4208, 7021, 54),
      "bomazi" -> (1198, 4492, 58),
      "ekera" -> (5719, 6555, 51),
      "faro" -> (5030, 5700, 57),
      "gunuku" -> (4994, 4286, 54),
      "honsi" -> (4042, 4588, 89),
      "itan" -> (5175, 3393, 48),
      "kaang" -> (5813, 3862, 62),
      "leza" -> (2691, 1561, 64),
      "mukuru" -> (661, 2380, 54),
      "nzame" -> (1670, 2706, 45),
      "orisha" -> (7060, 1327, 59),
      "pamba" -> (7403, 3123, 63),
      "shango" -> (6846, 2319, 63),
      "tore" -> (3017, 2272, 58),
      "wele" -> (436, 7040, 60),
      "monolith" -> (4515, 4105, 38),
      "peak" -> (3215, 5063, 579)
      )
    zones("z4").gates += (
      "gate1" -> (4702, 6768, 30),
      "gate2" -> (5515, 3368, 69),
      "gate3" -> (1564, 3356, 46),
      "gate4" -> (3889, 1118, 56),
      "geowarp1" -> (4202, 4325, 68),
      "geowarp2" -> (2384, 1925, 37)
      )
    zones("z4").locations += (
      "akkan" -> (2746, 4260, 39),
      "baal" -> (825, 5470, 72),
      "dagon" -> (1739, 5681, 40),
      "enkidu" -> (3217, 3574, 37),
      "girru" -> (4475, 5853, 78),
      "hanish" -> (3794, 5540, 89),
      "irkall" -> (4742, 5270, 66),
      "kusag" -> (6532, 4692, 46),
      "lahar" -> (6965, 5306, 38),
      "marduk" -> (3059, 2144, 70),
      "neti" -> (3966, 2417, 80),
      "zaqar" -> (4796, 2177, 75),
      "monolith" -> (5165, 4083, 35),
      "stonehenge" -> (4992, 3776, 56)
      )
    zones("z5").gates += (
      "gate1" -> (3432, 6498, 73),
      "gate2" -> (7196, 3917, 47),
      "gate3" -> (1533, 3540, 56),
      "gate4" -> (3197, 1390, 45),
      "geowarp1" -> (4899, 5633, 38),
      "geowarp2" -> (5326, 2558, 54)
      )
    zones("z5").locations += (
      "anu" -> (3479, 2556, 56),
      "bel" -> (3665, 4626, 58),
      "caer" -> (4570, 2601, 56),
      "dagd" -> (5825, 4449, 55),
      "eadon" -> (2725, 2853, 53),
      "gwydion" -> (5566, 3739, 61),
      "lugh" -> (6083, 5069, 72),
      "neit" -> (4345, 4319, 76),
      "ogma" -> (3588, 3227, 114),
      "pwyll" -> (4683, 4764, 104),
      "monolith" -> (3251, 3245, 160),
      "islands1" -> (6680, 6217, 125),
      "islands2" -> (1059, 6213, 120)
      )
    zones("z6").gates += (
      "gate1" -> (5040, 4327, 46),
      "gate2" -> (2187, 5338, 30),
      "gate3" -> (4960, 1922, 15),
      "gate4" -> (2464, 3088, 189),
      "geowarp1" -> (3221, 5328, 242),
      "geowarp2" -> (2237, 1783, 238)
      )
    zones("z6").locations += (
      "akna" -> (4509, 3732, 219),
      "anguta" -> (3999, 4170, 266),
      "igaluk" -> (3241, 5658, 235),
      "keelut" -> (3630, 1904, 265),
      "nerrivik" -> (3522, 3703, 322),
      "pinga" -> (5938, 3545, 96),
      "sedna" -> (3932, 5160, 232),
      "tarqaq" -> (2980, 2155, 237),
      "tootega" -> (5171, 3251, 217),
      "monolith" -> (4011, 4851, 32),
      "bridge" -> (3729, 4859, 234)
      )
    zones("z7").gates += (
      "gate1" -> (1516, 6448, 61),
      "gate2" -> (5249, 3819, 69),
      "gate3" -> (2763, 2961, 86),
      "gate4" -> (6224, 1152, 78),
      "geowarp1" -> (6345, 4802, 90),
      "geowarp2" -> (3800, 2197, 64)
      )
    zones("z7").locations += (
      "andvari" -> (3233, 7207, 78),
      "dagur" -> (4026, 6191, 60),
      "eisa" -> (3456, 4513, 75),
      "freyr" -> (2853, 3840, 56),
      "gjallar" -> (1056, 2656, 74),
      "helheim" -> (5542, 2532, 53),
      "jarl" -> (1960, 5462, 68),
      "kvasir" -> (4096, 1571, 69),
      "mani" -> (5057, 4989, 58),
      "nott" -> (6783, 4329, 46),
      "ran" -> (2378, 1919, 85),
      "vidar" -> (3772, 3024, 67),
      "ymir" -> (1911, 4008, 69),
      "monolith" -> (6390, 1622, 63)
      )
    zones("z8").gates += (
      "gate1" -> (5437, 5272, 32),
      "gate2" -> (3251, 5650, 60),
      "gate3" -> (5112, 2616, 40),
      "gate4" -> (2666, 1665, 45),
      "geowarp1" -> (3979, 5370, 47),
      "geowarp2" -> (6018, 3136, 35)
      )
    zones("z8").locations += (
     "atar" -> (3609, 2730, 47),
      "dahaka" -> (4633, 5379, 54),
      "hvar" -> (3857, 4764, 49),
      "izha" -> (5396, 3852, 51),
      "jamshid" -> (2371, 3378, 52),
      "mithra" -> (2480, 4456, 44),
      "rashnu" -> (3098, 3961, 59),
      "yazata" -> (4620, 3983, 62),
      "zal" -> (3966, 2164, 61),
      "arch1" -> (4152, 3285, 31),
      "arch2" -> (4688, 5272, 68),
      "pride" -> (2913, 4412, 63)
      )
    zones("z9").gates += (
      "gate1" -> (1505, 6981, 65),
      "gate2" -> (6835, 3517, 56),
      "gate3" -> (1393, 1376, 53),
      "geowarp1" -> (7081, 5552, 46),
      "geowarp2" -> (3776, 1092, 49)
      )
    zones("z9").locations += (
      "akua" -> (5258, 4041, 346),
      "drakulu" -> (3806, 2647, 151),
      "hiro" -> (4618, 5761, 190),
      "iva" -> (6387, 5199, 55),
      "karihi" -> (3879, 5574, 236),
      "laka" -> (4720, 6718, 49),
      "matagi" -> (5308, 5093, 239),
      "ngaru" -> (4103, 4077, 205),
      "oro" -> (4849, 4456, 208),
      "pele" -> (4549, 3712, 208),
      "rehua" -> (3843, 2195, 60),
      "sina" -> (5919, 2177, 91),
      "tara" -> (1082, 4225, 60),
      "wakea" -> (1785, 5241, 63),
      "monolith" -> (3246, 6507, 105)
      )
    zones("z10").gates += (
      "gate1" -> (6140, 6599, 71),
      "gate2" -> (4814, 4608, 59),
      "gate3" -> (3152, 3480, 54),
      "gate4" -> (1605, 1446, 40),
      "geowarp1" -> (3612, 6918, 38),
      "geowarp2" -> (3668, 3327, 55)
      )
    zones("z10").locations += (
      "azeban" -> (6316, 5160, 62),
      "cetan" -> (3587, 2522, 48),
      "heyoka" -> (4395, 2327, 47),
      "ikanam" -> (2740, 2412, 57),
      "kyoi" -> (5491, 2284, 62),
      "mekala" -> (6087, 2925, 59),
      "onatha" -> (3397, 5799, 48),
      "qumu" -> (3990, 5152, 46),
      "sungrey" -> (4609, 5624, 72),
      "tumas" -> (4687, 6392, 69),
      "verica" -> (4973, 3459, 47),
      "xelas" -> (6609, 4479, 56),
      "monolith" -> (5651, 6024, 38)
      )
    zones("home1").gates += (
      "gate1" -> (4158, 6344, 44),
      "gate2" -> (2214, 5797, 48),
      "gate3" -> (5032, 3241, 53)
      )
    zones("home1").locations += "hart_c" -> (2352, 5523, 66)
    zones("home2").gates += (
      "gate1" -> (5283, 4317, 44),
      "gate2" -> (3139, 4809, 40),
      "gate3" -> (3659, 2894, 26)
      )
    zones("home2").locations += "hart_c" -> (3126, 2864, 35)
    zones("home3").gates += (
      "gate1" -> (5657, 4681, 98),
      "gate2" -> (2639, 5366, 57),
      "gate3" -> (4079, 2467, 155)
      )
    zones("home3").locations += "hart_c" -> (3675, 2727, 91)
    zones("tzshtr").locations += "roof" -> (499, 1568, 25)
    zones("tzdrtr").locations += (
      "start" -> (2457, 1864, 23),
      "air_pad" -> (1700, 1900, 32)
      )
    zones("c1").gates += (
      "geowarp1" -> (998, 2038, 103),
      "geowarp2" -> (231, 1026, 82),
      "geowarp3" -> (2071, 1405, 102),
      "geowarp4" -> (1051, 370, 103)
      )
    zones("c2").gates += (
      "geowarp1" -> (999, 2386, 243),
      "geowarp2" -> (283, 1249, 172),
      "geowarp3" -> (1887, 1307, 192),
      "geowarp4" -> (1039, 155, 143)
      )
    zones("c3").gates += (
      "geowarp1" -> (1095, 1725, 25),
      "geowarp2" -> (226, 832, 42),
      "geowarp3" -> (1832, 1026, 43),
      "geowarp4" -> (981, 320, 46)
      )
    zones("c4").gates += (
      "geowarp1" -> (902, 1811, 93),
      "geowarp2" -> (185, 922, 113),
      "geowarp3" -> (1696, 1188, 92),
      "geowarp4" -> (887, 227, 115)
      )
    zones("c5").gates += (
      "geowarp1" -> (1195, 1752, 244),
      "geowarp2" -> (290, 1104, 235),
      "geowarp3" -> (1803, 899, 243),
      "geowarp4" -> (1042, 225, 246)
      )
    zones("c6").gates += (
      "geowarp1" -> (1067, 2044, 95),
      "geowarp2" -> (290, 693, 73),
      "geowarp3" -> (1922, 928, 33),
      "geowarp4" -> (1174, 249, 114)
      )
    zones("i3").gates += (
      "gate1" -> (1219, 2580, 30),
      "gate2" -> (2889, 2919, 33),
      "gate3" -> (2886, 1235, 32)
      )
    zones("i3").locations += (
      "dahaka" -> (1421, 2216, 30),
      "jamshid" -> (2500, 2543, 30),
      "izha" -> (2569, 1544, 30),
      "oasis" -> (2084, 1935, 40)
      )
    zones("i2").gates += (
      "gate1" -> (1243, 1393, 12),
      "gate2" -> (2510, 2544, 12),
      "gate3" -> (2634, 1477, 12)
      )
    zones("i2").locations += (
      "rashnu" -> (1709, 1802, 91),
      "sraosha" -> (2729, 2349, 91),
      "zal" -> (1888, 2728, 91),
      "center" -> (2082, 2192, 160)
      )
    zones("i1").gates += (
      "gate1" -> (1225, 2036, 67),
      "gate2" -> (2548, 2801, 65),
      "gate3" -> (2481, 1194, 89)
      )
    zones("i1").locations += (
      "hvar" -> (1559, 1268, 88),
      "mithra" -> (2855, 2850, 89),
      "yazata" -> (1254, 2583, 88),
      "south_of_volcano" -> (2068, 1686, 88)
      )
    zones("i4").gates += (
      "gate1" -> (2359, 2717, 36),
      "gate2" -> (2732, 1355, 36),
      "geowarp" -> (1424, 1640, 45)
      )
    zones("i4").locations += "atar" -> (1981, 1886, 36)
  }
}
