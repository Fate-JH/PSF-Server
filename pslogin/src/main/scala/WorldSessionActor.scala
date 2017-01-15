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
import net.psforever.packet.game.objectcreate._
import net.psforever.types.{ChatMessageType, Vector3}
import scodec.Attempt

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
  //val objectHex = hex"18 570C0000 BC8 4B00 6C2D7 65535 CA16 0 00 0 13 4 40 00 0970 49006C006C006C004900490049006C006C006C0049006C0049006C006C0049006C006C006C0049006C006C004900 84 52 70 76 1E 80 80 00 00 00 00 00 3FFFC 0 00 00 00 20 00 00 0 FF 6A 7 03 FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFD 90 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 90 01 90 00 64 00 00 01 00 7E C8 00 C8 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 C0 00 42 C5 46 86 C7 00 00 00 80 00 00 12 40 7870655F73616E6374756172795F68656C70 90 7870655F74685F666972656D6F646573 8B 757365645F6265616D6572 85 6D61703133 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 0A 23 02 60 04 04 40 00 00 10 00 06 02 08 14 D0 08 0C 80 00 02 00 02 6B 4E 00 82 88 00 00 02 00 00 C0 41 C0 9E 01 01 90 00 00 64 00 44 2A 00 10 91 00 00 00 40 00 18 08 38 94 40 20 32 00 00 00 80 19 05 48 02 17 20 00 00 08 00 70 29 80 43 64 00 00 32 00 0E 05 40 08 9C 80 00 06 40 01 C0 AA 01 19 90 00 00 C8 00 3A 15 80 28 72 00 00 19 00 04 0A B8 05 26 40 00 03 20 06 C2 58 00 A7 88 00 00 02 00 00 80 00 00"
  //val objectHex = hex"18 570C0000 BC8 4B00 00F00 00F01 8014 0 00 0 20 2 40 00 0970 49006C006C006C004900490049006C006C006C0049006C0049006C006C0049006C006C006C0049006C006C004900 82 01 40 76 1E 80 80 00 00 00 00 00 3FFFC 0 00 00 00 20 00 00 0 10 00 3 03 FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFC 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 03 FF FF FF FD FF FE 00 01 00 0F FF FF FF FE 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 C0 00 42 C5 46 86 C7 00 00 00 80 00 00 12 40 7870655F73616E6374756172795F68656C70 90 7870655F74685F666972656D6F646573 8B 757365645F6265616D6572 85 6D61703133 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 01 0A 23 02 60 04 04 40 00 00 10 00 06 02 08 14 D0 08 0C 80 00 02 00 02 6B 4E 00 82 88 00 00 02 00 00 C0 41 C0 9E 01 01 90 00 00 64 00 44 2A 00 10 91 00 00 00 40 00 18 08 38 94 40 20 32 00 00 00 80 19 05 48 02 17 20 00 00 08 00 70 29 80 43 64 00 00 32 00 0E 05 40 08 9C 80 00 06 40 01 C0 AA 01 19 90 00 00 C8 00 3A 15 80 28 72 00 00 19 00 04 0A B8 05 26 40 00 03 20 06 C2 59 00 A7 88 00 00 02 00 00 80 00 00"
  //currently, the character's starting BEP is discarded due to unknown bit format
  val app = CharacterAppearanceData(
    Vector3(674.8438f, 726.789f, 91.15625f),
    19,
    1,
    false,
    4,
    "IlllIIIlllIlIllIlllIllI",
    4,
    2,
    2, 9,
    1,
    3, 118, 30, 0x8080, 0xFFFF, 2,
    255, 106, 7,
    RibbonBars()
  )
  val inv =
    InventoryItem(ObjectClass.BEAMER, PlanetSideGUID(76), 0, WeaponData(8, ObjectClass.ENERGY_CELL, PlanetSideGUID(77), 0, AmmoBoxData(16))) ::
      InventoryItem(ObjectClass.SUPPRESSOR, PlanetSideGUID(78), 2, WeaponData(8, ObjectClass.BULLETS_9MM, PlanetSideGUID(79), 0, AmmoBoxData(25))) ::
      InventoryItem(ObjectClass.FORCE_BLADE, PlanetSideGUID(80), 4, WeaponData(8, ObjectClass.FORCE_BLADE_AMMO, PlanetSideGUID(81), 0, AmmoBoxData(1))) ::
      InventoryItem(ObjectClass.SLOT_BLOCKER, PlanetSideGUID(82), 5, AmmoBoxData(1)) ::
      InventoryItem(ObjectClass.BULLETS_9MM, PlanetSideGUID(83), 6, AmmoBoxData(50)) ::
      InventoryItem(ObjectClass.BULLETS_9MM, PlanetSideGUID(84), 9, AmmoBoxData(50)) ::
      InventoryItem(ObjectClass.BULLETS_9MM, PlanetSideGUID(85), 12, AmmoBoxData(50)) ::
      InventoryItem(ObjectClass.BULLETS_9MM_AP, PlanetSideGUID(86), 33, AmmoBoxData(50)) ::
      InventoryItem(ObjectClass.ENERGY_CELL, PlanetSideGUID(87), 36, AmmoBoxData(50)) ::
      InventoryItem(ObjectClass.REK, PlanetSideGUID(88), 39, REKData(8)) ::
      Nil
  val obj = CharacterData(
    app,
    100, 100,
    50,
    1, 7, 7,
    100, 100,
    28, 4, 44, 84, 104, 1900,
    "xpe_sanctuary_help" :: "xpe_th_firemodes" :: "used_beamer" :: "map13" :: Nil,
    List.empty,
    InventoryData(
      true, false, false, inv
    )
  )
  val objectHex = ObjectCreateMessage(0, ObjectClass.AVATAR, PlanetSideGUID(75), obj)

  val app2 = CharacterAppearanceData(
    Vector3(3674.8438f, 2726.789f, 91.15625f),
    19,
    1,
    false,
    4,
    "Variable Dummmy",
    4,
    2,
    2, 9,
    1,
    3, 118, 30, 0x8080, 0xFFFF, 2,
    0, 64, 7,
    RibbonBars()
  )
  val obj2 = CharacterData(
    app2,
    100, 100,
    50,
    1, 7, 7,
    100, 100,
    28, 4, 44, 84, 104, 1900,
    List.empty,
    List.empty,
    InventoryData(
      true, false, false, List.empty
    )
  )
  val objectHex2 = ObjectCreateMessage(0, ObjectClass.AVATAR, PlanetSideGUID(89), obj2)
//  val app3 = CharacterAppearanceData(
//    Vector3(3674.8438f, 2728.789f, 91.15625f),
//    19,
//    1,
//    false,
//    4,
//    "Control Dummy",
//    4,
//    2,
//    2, 9,
//    1,
//    3, 118, 30, 0x8080, 0xFFFF, 2,
//    0, 64, 7,
//    RibbonBars()
//  )
//  val obj3 = CharacterData(
//    app3,
//    100, 100,
//    50,
//    1, 7, 7,
//    100, 100,
//    28, 4, 44, 84, 104, 1900,
//    List.empty,
//    List.empty,
//    InventoryData(
//      true, false, false, List.empty
//    )
//  )
//  val objectHex3 = ObjectCreateMessage(0, ObjectClass.AVATAR, PlanetSideGUID(90), obj3)

  var ang = 0

  def handleGamePkt(pkt : PlanetSideGamePacket) = pkt match {
    case ConnectToWorldRequestMessage(server, token, majorVersion, minorVersion, revision, buildDate, unk) =>

      val clientVersion = s"Client Version: ${majorVersion}.${minorVersion}.${revision}, ${buildDate}"

      log.info(s"New world login to ${server} with Token:${token}. ${clientVersion}")

      // ObjectCreateMessage
      sendResponse(PacketCoding.CreateGamePacket(0, objectHex))
      sendResponse(PacketCoding.CreateGamePacket(0, CharacterInfoMessage(PlanetSideZoneID(10), 41605313, PlanetSideGUID(75), false, 0)))

      // NOTE: PlanetSideZoneID just chooses the background
      sendResponse(PacketCoding.CreateGamePacket(0,
        CharacterInfoMessage(PlanetSideZoneID(1), 0, PlanetSideGUID(0), true, 0)))
    case msg @ CharacterRequestMessage(charId, action) =>
      log.info("Handling " + msg)

      action match {
        case CharacterRequestAction.Delete =>
          sendResponse(PacketCoding.CreateGamePacket(0, ActionResultMessage(false, Some(1))))
        case CharacterRequestAction.Select =>
          objectHex match {
            case obj @ ObjectCreateMessage(len, cls, guid, _, _) =>
              log.debug("Object: " + obj)
              sendResponse(PacketCoding.CreateGamePacket(0, ReplicationStreamMessage(5, Some(6), Some(false), Vector(
                    SquadListing(0, Some(SquadHeader(131,false,None, SquadInfo("00","0",PlanetSideGUID(1),10,10, PlanetSideGUID(65535))))),
//                    SquadListing(1, Some(SquadHeader(131,false,None, SquadInfo("01","1",PlanetSideGUID(1),10,10, PlanetSideGUID(22))))),
//                    SquadListing(2, Some(SquadHeader(131,false,None, SquadInfo("02","2",PlanetSideGUID(1),10,10, PlanetSideGUID(23))))),
//                    SquadListing(3, Some(SquadHeader(131,false,None, SquadInfo("03","3",PlanetSideGUID(1),10,10, PlanetSideGUID(24))))),
//                    SquadListing(4, Some(SquadHeader(131,false,None, SquadInfo("04","4",PlanetSideGUID(1),10,10, PlanetSideGUID(25))))),
//                    SquadListing(5, Some(SquadHeader(131,false,None, SquadInfo("05","5",PlanetSideGUID(1),10,10, PlanetSideGUID(26))))),
//                    SquadListing(6, Some(SquadHeader(131,false,None, SquadInfo("06","6",PlanetSideGUID(1),10,10, PlanetSideGUID(27))))),
//                    SquadListing(7, Some(SquadHeader(131,false,None, SquadInfo("07","7",PlanetSideGUID(1),10,10, PlanetSideGUID(28))))),
//                    SquadListing(8, Some(SquadHeader(131,false,None, SquadInfo("08","8",PlanetSideGUID(1),10,10, PlanetSideGUID(29))))),
//                    SquadListing(9, Some(SquadHeader(131,false,None, SquadInfo("09","9",PlanetSideGUID(1),10,10, PlanetSideGUID(30))))),
//                    SquadListing(10, Some(SquadHeader(131,false,None, SquadInfo("10","10",PlanetSideGUID(1),10,10, PlanetSideGUID(31))))),
//                    SquadListing(11, Some(SquadHeader(131,false,None, SquadInfo("11","11",PlanetSideGUID(1),10,10, PlanetSideGUID(32))))),
//                    SquadListing(12, Some(SquadHeader(131,false,None, SquadInfo("12","12",PlanetSideGUID(1),10,10, PlanetSideGUID(33))))),
//                    SquadListing(13, Some(SquadHeader(131,false,None, SquadInfo("13","13",PlanetSideGUID(1),10,10, PlanetSideGUID(34))))),
//                    SquadListing(14, Some(SquadHeader(131,false,None, SquadInfo("14","14",PlanetSideGUID(1),10,10, PlanetSideGUID(35))))),
//                    SquadListing(15, Some(SquadHeader(131,false,None, SquadInfo("15","15",PlanetSideGUID(1),10,10, PlanetSideGUID(36))))),
//                    SquadListing(16, Some(SquadHeader(131,false,None, SquadInfo("16","16",PlanetSideGUID(1),10,10, PlanetSideGUID(37))))),
                    SquadListing(255)
                  )
                ))
              )

              // LoadMapMessage 13714 in mossy .gcap
              // XXX: hardcoded shit
              sendResponse(PacketCoding.CreateGamePacket(0, LoadMapMessage("ugd03","c3",40100,25,true,3770441820L))) //VS Sanctuary
              sendResponse(PacketCoding.CreateGamePacket(0, ZonePopulationUpdateMessage(PlanetSideGUID(13), 414, 138, 0, 138, 0, 138, 0, 138, 0)))
              sendResponse(PacketCoding.CreateGamePacket(0, objectHex))
              sendResponse(PacketCoding.CreateGamePacket(0, objectHex2))
              //sendResponse(PacketCoding.CreateGamePacket(0, objectHex3))

              // These object_guids are specfic to VS Sanc
              sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(2), PlanetSideEmpire.VS))) //HART building C
              sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(29), PlanetSideEmpire.NC))) //South Villa Gun Tower

              sendResponse(PacketCoding.CreateGamePacket(0, TimeOfDayMessage(0, 4653056, 0, 0, 32, 65)))
              sendResponse(PacketCoding.CreateGamePacket(0, ContinentalLockUpdateMessage(PlanetSideGUID(13), PlanetSideEmpire.VS))) // "The VS have captured the VS Sanctuary."
              sendResponse(PacketCoding.CreateGamePacket(0, BroadcastWarpgateUpdateMessage(PlanetSideGUID(13), PlanetSideGUID(1), 32))) // VS Sanctuary: Inactive Warpgate -> Broadcast Warpgate

//              sendResponse(PacketCoding.CreateGamePacket(0,BuildingInfoUpdateMessage(
//                PlanetSideGUID(6),   //Ceryshen
//                PlanetSideGUID(2),   //Anguta
//                8,                   //80% NTU
//                true,                //Base hacked
//                PlanetSideEmpire.NC, //Base hacked by NC
//                600000,              //10 minutes remaining for hack
//                PlanetSideEmpire.VS, //Base owned by VS
//                0,                   //!! Field != 0 will cause malformed packet. See class def.
//                PlanetSideGeneratorState.Critical, //Generator critical
//                true,                //Respawn tubes destroyed
//                true,                //Force dome active
//                16,                  //Tech plant lattice benefit
//                0,
//                0,                   //!! Field > 0 will cause malformed packet. See class def.
//                0,
//                false,
//                8,                   //!! Field != 8 will cause malformed packet. See class def.
//                true,                //Boosted spawn room pain field
//                true)))              //Boosted generator room pain field
//             sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(23), PlanetSideGUID(10175),
//                10,false,PlanetSideEmpire.NC,0,PlanetSideEmpire.NC,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(8),
//                9,false,PlanetSideEmpire.NC,0,PlanetSideEmpire.NC,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(7),
//                8,false,PlanetSideEmpire.TR,0,PlanetSideEmpire.TR,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(28), PlanetSideGUID(10257),
//                7,false,PlanetSideEmpire.VS,0,PlanetSideEmpire.VS,0,PlanetSideGeneratorState.Normal,false,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(28), PlanetSideGUID(10259),
//                6,false,PlanetSideEmpire.NC,0,PlanetSideEmpire.NC,0,PlanetSideGeneratorState.Normal,false,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(28), PlanetSideGUID(10304),
//                5,false,PlanetSideEmpire.TR,0,PlanetSideEmpire.TR,0,PlanetSideGeneratorState.Normal,false,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(4),
//                4,false,PlanetSideEmpire.VS,0,PlanetSideEmpire.VS,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(5),
//                3,false,PlanetSideEmpire.NC,0,PlanetSideEmpire.NC,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(6),
//                2,false,PlanetSideEmpire.TR,0,PlanetSideEmpire.TR,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(1),
//                1,false,PlanetSideEmpire.VS,0,PlanetSideEmpire.VS,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(2),
//                1,false,PlanetSideEmpire.NC,0,PlanetSideEmpire.NC,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))
//              sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(1), PlanetSideGUID(3),
//                1,false,PlanetSideEmpire.TR,0,PlanetSideEmpire.TR,0,PlanetSideGeneratorState.Normal,true,true,0,0,0,0,false,8,true,true)))

              sendRawResponse(hex"BE 88 4D B5 6C 46 AE D0 A0 04 00")
              sendRawResponse(hex"08 4B 00 84 DB 56 C4 6A ED 0A 00 00 00 00 10 1B 83 04 1F F6 8A BE 5D 57 A2 00 00 00 00 AF 00 0F 00 1B 82 04 1F FB CA BF 96 AF 02 00 00 00 00 AF 00 0F 00 1B 81 04 1F E6 F4 BF 32 F7 22 00 00 00 00 AF 00 0F 00")
              sendResponse(PacketCoding.CreateGamePacket(0, SetCurrentAvatarMessage(guid,0,0)))
              sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_EXPANSIONS,true,"","1 on",None)))

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
      if(is_crouching && !ArmorChangedMessage.changeOnce) {
        ArmorChangedMessage.changeOnce = is_crouching
        ang += 1
//        val pkt = PlayerStateMessage(
//          PlanetSideGUID(89),
//          Vector3(3674.8438f, 2726.789f, 91.15625f),
//          Some(Vector3(0.0f,0.0f,0.0f)),
//          0, 0, 0, 0,
//          true, false, false, false, false
//        )
//        sendResponse(PacketCoding.CreateGamePacket(0, pkt))
     // sendRawResponse(hex"08 5900 DFD17 B5AEB 380B 0F 80 00 29 90")
        //sendRawResponse(hex"BE 87 0C BC E7 A1 34 50 64 24 00")
        //carefully delete inventory
        sendRawResponse(hex"19 4C00 00") //beamer
        sendRawResponse(hex"19 4D00 00") //beamer ammo
        sendRawResponse(hex"19 4E00 00") //suppressor
        sendRawResponse(hex"19 4F00 00") //suppressor ammo
        sendRawResponse(hex"19 5000 00") //forceblade
        sendRawResponse(hex"19 5100 00") //forceblade ammo?
        sendRawResponse(hex"19 5300 00") //ammo, 9mm
        sendRawResponse(hex"19 5400 00") //ammo, 9mm
        sendRawResponse(hex"19 5500 00") //ammo, 9mm
        sendRawResponse(hex"19 5600 00") //ammo, 9mm ap
        sendRawResponse(hex"19 5700 00") //ammo, plasma
        sendRawResponse(hex"19 5800 00") //rek
//        sendResponse(PacketCoding.CreateGamePacket(0, ArmorChangedMessage(avatar_guid, 2, 1)))
//        //see capture "last", starting @ line 688
//        //note: adding a medkit creates the shortcut if it doesn't exist and dispatches an 0x28 packet to the server
//        //sendRawResponse(hex"18 7C000000 2580 692 5C0F 9E C0000018000") //reaver fury rockets, 2,6
//        sendRawResponse(hex"18 7C000000 2580 79A 0D06 86 C8000020000") //buckshot, 0,0
//        sendRawResponse(hex"18 7C000000 2580 0E0 5300 A1   C80001FFFE0") //9mm, 3,0
//        sendRawResponse(hex"18 7C000000 2580 0E0 1506 BC C8000064000") //9mm, 6,0
//        sendRawResponse(hex"18 7C000000 2580 0C2 F805 A6 C8000002000") //medkit, 3,5
//        sendRawResponse(hex"18 7C000000 2580 0C2 F604 B8 C8000002000") //medkit, 5,5
//        //sendRawResponse(hex"18 87000000 2580 100 690B 80 8800000200008") // ACE, Boomer, pistol slot 1
//        sendRawResponse(hex"18 7C000000 2580 0C2 1A06 CA C8000002000") //medkit, 7,5
//        sendRawResponse(hex"18 DC000000 2580 542 4407 80 480000020000C04A941A0B019000000C000") // plasma grenades, pistol slot 1
//        sendRawResponse(hex"18 97000000 2580 6C2 9F05 81 48000002000080000") //rek, pistol slot 2
//        sendRawResponse(hex"18 DC000000 2580 501 6A07 B6 480000020000C04A137A0B019000000C000") // jammer grenades, 5,3
//        sendRawResponse(hex"18 DC000000 2580 501 4406 C8 480000020000C04A13C209019000000C000") // jammer grenades, 7,3
//        sendRawResponse(hex"18 DC000000 2580 2C9 B905 82 480000020000C041C00C0B0190000078000") // gauss, rifle slot 1
//        sendRawResponse(hex"18 DC000000 2580 181 F804 89 480000020000C04F35AE0D0190000030000") // sweeper, 0,3
        //val string = hex"18 27010000 2580 612 a706 82 080000020000c08 1c13a0d0190000078000 13a4701a072000000800" //punisher
        val obj = ConcurrentFeedWeaponData(0, AmmoBoxData(28, PlanetSideGUID(1693), 0, AmmoBoxData(30)) :: AmmoBoxData(413, PlanetSideGUID(1564), 1, AmmoBoxData(1)) :: Nil)
        val msg = ObjectCreateMessage(0, 706, PlanetSideGUID(1703), ObjectCreateMessageParent(PlanetSideGUID(75), 2), obj)
        sendResponse(PacketCoding.CreateGamePacket(0, msg))
      }

    case msg @ ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents) =>
      // TODO: Prevents log spam, but should be handled correctly
      if (messagetype != ChatMessageType.CMT_TOGGLE_GM) {
        log.info("Chat: " + msg)
      }

      if(messagetype == ChatMessageType.CMT_VOICE) {
        log.info("Voice message.")
        sendRawResponse(hex"6b18e665702e200080f1fc")
      }

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

    case msg @ AvatarGrenadeStateMessage(player_guid, state) =>
      log.info("AvatarGrenadeStateMessage: " + msg)

    case msg @ SplashHitMessage(bytes) =>
      log.info("SplashHitMessage: " + bytes.toString)

    case msg @ ZipLineMessage(player_guid, origin_side, action, id, unk4, unk5, unk6) =>
      log.info("ZipLineMessage: " + msg)
      if(action == 0) {
        //sendResponse(PacketCoding.CreateGamePacket(0, msg))
      }
      else if(action == 1) {
        //disembark from zipline at destination?
        //sendResponse(PacketCoding.CreateGamePacket(0, ZipLineMessage(player_guid, origin_side, 3, id, unk4, unk5, unk6)))
      }
      else if(action == 2) {
        //get off by force
      }

    case msg @ ProximityTerminalUseMessage(player_guid, object_guid, unk) =>
      log.info("ProximityTerminalUseMessage: " + msg)

    case msg @ MountVehicleMsg(player_guid, vehicle_guid, seat) =>
      log.info("MountVehicleMsg: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(vehicle_guid, player_guid, 0)))

    case msg @ DismountBuildingMsg(player_guid, building_guid) =>
      log.info("DismountBuildingMsg: " + msg)

    case msg @ GenericActionMessage(action) =>
      log.info("GenericActionMessage: " + msg)

    case msg @ HitHint(unk1, unk2, x) =>
      log.info("HitHint: " + msg)

    case msg @ WarpgateRequest(continent_guid, building_guid, dest_building_guid, dest_continent_guid, unk1, unk2) =>
      log.info("WarpgateRequest:" + msg)

    case msg @ SquadDefinitionActionMessage(bytes) =>
      log.info("SquadDefinitionActionMessage: " + msg)

    case msg @ GenericCollisionMsg(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
      log.info("GenericCollision: "+msg)

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
}
