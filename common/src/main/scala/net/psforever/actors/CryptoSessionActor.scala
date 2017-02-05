// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors

import akka.actor.{Actor, ActorRef, MDCContextAware}
import net.psforever.crypto.CryptoInterface.CryptoStateWithMAC
import net.psforever.crypto.CryptoInterface
import net.psforever.packet._
import scodec.Attempt.{Failure, Successful}
import scodec.bits._
import java.security.SecureRandom

import net.psforever.packet.control.{ClientStart, ServerStart, TeardownConnection}
import net.psforever.packet.crypto._
import net.psforever.packet.game.PingMsg
import org.log4s.MDC
import MDCContextAware.Implicits._
import net.psforever.actors.crypto.{CryptoSessionAPI, DropCryptoSession}
import net.psforever.actors.session.{RawPacket, ResponsePacket, SessionRouterAPI}
import net.psforever.actors.udp.HelloFriend

/**
  * This `Actor` accepts cryptographic information seeded from the client.
  * Subsequently, cryptographic data on the server is combined and that produces the credentials for an active "crypto state."
  * These credentials are used to encode network-bound packets and decrypt receiving packets.
  * Primarily, after the "crypto state" has been established, it becomes just another relay in the messaging pipeline.
  */
class CryptoSessionActor extends Actor with MDCContextAware {
  private[this] val log = org.log4s.getLogger("CryptoSessionActor")

  var sessionId : Long = 0
  var leftRef : ActorRef = ActorRef.noSender
  var rightRef : ActorRef = ActorRef.noSender
  var cryptoDHState : Option[CryptoInterface.CryptoDHState] = None
  var cryptoState : Option[CryptoInterface.CryptoStateWithMAC] = None
  val random = new SecureRandom()
  //crypto handshake state
  var serverNonce : Long = 0
  var serverChallenge : ByteVector = ByteVector.empty
  var serverChallengeResult : ByteVector = ByteVector.empty
  var serverMACBuffer : ByteVector = ByteVector.empty
  var clientNonce : Long = 0
  var clientPublicKey : ByteVector = ByteVector.empty
  var clientChallenge : ByteVector = ByteVector.empty
  var clientChallengeResult : ByteVector = ByteVector.empty

  /**
    * The initial behavior demonstrated by this `Actor`.
    * Pass to the method `initialize`.
    * @return a partial function
    */
  def receive = Initializing

  /**
    * The initial behavior demonstrated by this `Actor`.
    * Upon being greeted, the `Session` begins handling packets that are passed in between parent and child `Actors.`
    * @return a partial function
    * @see `NewClient`
    */
  def Initializing : Receive = {
    case HelloFriend(sessionid, right) =>
      import MDCContextAware.Implicits._
      this.sessionId = sessionid
      leftRef = sender()
      rightRef = right.asInstanceOf[ActorRef]
      rightRef !> HelloFriend(sessionid, self) //whomever we send to has to send something back to us
      log.trace(s"Left sender ${leftRef.path.name}")
      context.become(NewClient)

    case default =>
      log.error("Unknown message " + default)
      context.stop(self)
  }

  /**
    * After the initial function has been successfully progressed, this behavior is moved to the top of the stack.
    * Subsequent activity is handled by the guidelines of this behavior.
    * At first our cryptographical protocol has not been established, but we are still listening for only packets of that sort.
    * The `ControlPacket` that is anticipated should discuss the client starting.
    * @return a partial function
    * @see `CryptoExchange`
    */
  def NewClient : Receive = {
    case RawPacket(msg) =>
      PacketCoding.UnmarshalPacket(msg) match {
        case Successful(p) =>
          log.trace("Initializing -> NewClient")
          p match {
            case ControlPacket(_, ClientStart(nonce)) =>
              clientNonce = nonce
              serverNonce = Math.abs(random.nextInt())
              sendResponse(PacketCoding.CreateControlPacket(ServerStart(nonce, serverNonce)))
              context.become(CryptoExchange)

            case default =>
              log.error(s"Unexpected packet type $p in state NewClient")
          }

        case Failure(e) =>
          /*
          There is a special case where no crypto is being used.
          The only packet coming through looks like PingMsg.
          This is a hardcoded feature of the client @ 0x005FD618
           */
          PacketCoding.DecodePacket(msg) match {
            case Successful(packet) =>
              packet match {
                case ping @ PingMsg(_, _) =>
                  sendResponse(ping) //reflect the packet back to the sender

                case default =>
                  log.error(s"Unexpected non-crypto packet type $packet in state NewClient")
              }

            case Failure(err) =>
              log.error(s"Could not decode packet for reason: $err in state NewClient")
          }
      }

    case default =>
      log.error(s"Invalid message '$default' received in state NewClient")
  }

  /**
    * After initializing our cryptographic state, we are now prepared to configure it properly.
    * This "challenge packet" contains the seeding information that is necessary for building the appropriate response.
    * Most of the inputs and intermediate objects are saved for later use.
    * @return a partial function
    * @see `CryptoSetupFinishing`
    */
  def CryptoExchange : Receive = {
    case RawPacket(msg) =>
      PacketCoding.UnmarshalPacket(msg, CryptoPacketOpcode.ClientChallengeXchg) match {
        case Failure(e) =>
          log.error("Could not decode packet in state CryptoExchange: " + e)

        case Successful(p) =>
          log.trace("NewClient -> CryptoExchange")
          p match {
            case CryptoPacket(seq, ClientChallengeXchg(time, challenge, p2, g)) =>
              cryptoDHState = Some(new CryptoInterface.CryptoDHState())
              val dh = cryptoDHState.get
              dh.start(p2, g) //initialize our crypto state from the client's P and G
              clientChallenge = ServerChallengeXchg.getCompleteChallenge(time, challenge) //save the client challenge
              serverMACBuffer ++= msg.drop(3) //save the packet we got for a MAC check later. drop the first 3 bytes
              val serverTime = System.currentTimeMillis() / 1000L
              val randomChallenge = getRandBytes(0xc)
              serverChallenge = ServerChallengeXchg.getCompleteChallenge(serverTime, randomChallenge) //store the complete server challenge for later
              val packet = PacketCoding.CreateCryptoPacket(seq, ServerChallengeXchg(serverTime, randomChallenge, dh.getPublicKey))
              val sentPacket = sendResponse(packet)
              serverMACBuffer ++= sentPacket.drop(3) //save the sent packet a MAC check
              context.become(CryptoSetupFinishing)

            case default =>
              log.error(s"Unexpected packet type $p in state CryptoExchange")
          }
      }
    case default =>
      log.error(s"Invalid message '$default' received in state CryptoExchange")
  }

  /**
    * The final stage of properly configuring our cryptographic state.
    * Combining information previously collected and newly-gathered information, produce a series of `ByteVector`s.
    * The `ByteVector`s are transformed into a series of keys.
    * These keys complete the configuration of our crypto state.
    * @return a partial function
    * @see `Established`
    */
  def CryptoSetupFinishing : Receive = {
    case RawPacket(msg) =>
      PacketCoding.UnmarshalPacket(msg, CryptoPacketOpcode.ClientFinished) match {
        case Failure(e) =>
          log.error("Could not decode packet in state CryptoSetupFinishing: " + e)

        case Successful(p) =>
          log.trace("CryptoExchange -> CryptoSetupFinishing")
          p match {
            case CryptoPacket(seq, ClientFinished(clientPubKey, clientChalResult)) =>
              clientPublicKey = clientPubKey
              clientChallengeResult = clientChalResult
              serverMACBuffer ++= msg.drop(3) //save the packet we got for a MAC check later
              val dh = cryptoDHState.get
              val agreedValue = dh.agree(clientPublicKey)
              dh.close //we are now done with the DH crypto object

              val agreedMessage = ByteVector("master secret".getBytes) ++ clientChallenge ++
                hex"00000000" ++ serverChallenge ++ hex"00000000"
              val masterSecret = CryptoInterface.MD5MAC(agreedValue,
                agreedMessage,
                20)
              serverChallengeResult = CryptoInterface.MD5MAC(masterSecret,
                ByteVector("server finished".getBytes) ++ serverMACBuffer ++ hex"01",
                0xc)
              val clientChallengeResultCheck = CryptoInterface.MD5MAC(masterSecret,
                ByteVector("client finished".getBytes) ++ serverMACBuffer ++ hex"01" ++ clientChallengeResult ++ hex"01",
                0xc)
              val decExpansion = ByteVector("client expansion".getBytes) ++ hex"0000" ++ serverChallenge ++
                hex"00000000" ++ clientChallenge ++ hex"00000000"
              val encExpansion = ByteVector("server expansion".getBytes) ++ hex"0000" ++ serverChallenge ++
                hex"00000000" ++ clientChallenge ++ hex"00000000"
              /*
               Expand the encryption and decryption keys
               The first 20 bytes are for RC5, and the next 16 are for the MAC'ing keys
                */
              val expandedDecKey = CryptoInterface.MD5MAC(masterSecret,
                decExpansion,
                0x40) //this is what is visible in IDA
              val expandedEncKey = CryptoInterface.MD5MAC(masterSecret,
                encExpansion,
                0x40)
              val decKey = expandedDecKey.take(20)
              val encKey = expandedEncKey.take(20)
              val decMACKey = expandedDecKey.drop(20).take(16)
              val encMACKey = expandedEncKey.drop(20).take(16)

              cryptoState = Some(new CryptoStateWithMAC(decKey, encKey, decMACKey, encMACKey)) //spin up our encryption program
              val packet = PacketCoding.CreateCryptoPacket(seq, ServerFinished(serverChallengeResult))
              sendResponse(packet)
              context.become(Established)

            case default =>
              failWithError(s"Unexpected packet type $default in state CryptoSetupFinished")
          }
      }
    case default =>
      failWithError(s"Invalid message '$default' received in state CryptoSetupFinished")
  }

  /**
    * After the cryptographic state is set up, this behavior is moved to the top of the stack.
    * Subsequent activity is handled by the guidelines of this behavior, using the state calculated by the previous work.
    * Raw messages are either encrypted and bound for the outside network or decrypted and fed back into the pipeline.
    * Other packets are dealt with according to their identity.
    * @return a partial function
    */
  def Established : Receive = {
    case RawPacket(msg) =>
      if(sender() == rightRef) {
        val packet = PacketCoding.encryptPacket(cryptoState.get, 0, msg).require
        sendResponse(packet)
      }
      else {
        PacketCoding.UnmarshalPacket(msg) match {
          case Successful(p) =>
            p match {
              case encPacket @ EncryptedPacket(seq, _) =>
                PacketCoding.decryptPacket(cryptoState.get, encPacket) match {
                  case Successful(packet) =>
                    self !> packet

                  case Failure(e) =>
                    log.error("Failed to decode encrypted packet: " + e)
                }

              case default =>
                failWithError(s"Unexpected packet type $default in state Established")

            }

          case Failure(e) =>
            log.error("Could not decode raw packet: " + e)
        }
      }

    case api : CryptoSessionAPI =>
      api match {
        case DropCryptoSession() =>
          handleEstablishedPacket(
            sender(),
            PacketCoding.CreateControlPacket(TeardownConnection(clientNonce))
          )
      }

    case ctrl @ ControlPacket(_, _) =>
      val from = sender()
      handleEstablishedPacket(from, ctrl)

    case game @ GamePacket(_, _, _) =>
      val from = sender()
      handleEstablishedPacket(from, game)

    case sessionAPI : SessionRouterAPI =>
      leftRef !> sessionAPI

    case default =>
      failWithError(s"Invalid message '$default' received in state net.psforever.actors.Established")
  }

  /**
    * What happens when this `Actor` stops.
    * Don't leak crypto object memory even on an `Exception`.
    */
  override def postStop() : Unit = {
    cleanupCrypto()
  }

  /**
    * Log the problem that was encountered.
    * @param error description of the error
    */
  def failWithError(error : String) : Unit = {
    log.error(error)
  }

  /**
    * Blank all data related to the crypto state.
    */
  private def cleanupCrypto() = {
    if(cryptoDHState.isDefined) {
      cryptoDHState.get.close
      cryptoDHState = None
    }
    if(cryptoState.isDefined) {
      cryptoState.get.close
      cryptoState = None
    }
  }

  /**
    * Blank all data related to the communication that established the crypto state.
    */
  private def resetState() : Unit = {
    context.become(receive)
    cleanupCrypto() //reset the crypto primitives
    serverChallenge = ByteVector.empty
    serverChallengeResult = ByteVector.empty
    serverMACBuffer = ByteVector.empty
    clientPublicKey = ByteVector.empty
    clientChallenge = ByteVector.empty
    clientChallengeResult = ByteVector.empty
  }

  /**
    * When a more conventional non-cryptographic packet comes across the pipeline, pass it on.
    * Only packets originating from this same `Actor` or whatever is the `rightActor` are allowed to be propagated.
    * @param from where from this communication originated
    * @param cont the received packet
    */
  def handleEstablishedPacket(from : ActorRef, cont : PlanetSidePacketContainer) : Unit = {
    if(from == self) { //we are processing a packet we decrypted
      rightRef !> cont
    }
    else if(from == rightRef) { //processing a completed packet from the right. encrypt
      val packet = PacketCoding.encryptPacket(cryptoState.get, cont).require
      sendResponse(packet)
    }
    else {
      log.error(s"Invalid sender when handling a message in Established $from")
    }
  }

  /**
    * Take an indeterminate packet, marshall it into something known, and dispatch it.
    * @param cont the unknown packet
    * @return the byte representation of the packet that was marshalled
    */
  def sendResponse(cont : PlanetSidePacketContainer) : ByteVector = {
    log.trace("CRYPTO SEND: " + cont)
    val pkt = PacketCoding.MarshalPacket(cont)
    pkt match {
      case Failure(e) =>
        log.error(s"Failed to marshal packet ${cont.getClass.getName} when sending response")
        ByteVector.empty

      case Successful(v) =>
        val bytes = v.toByteVector
        MDC("sessionId") = sessionId.toString
        leftRef !> ResponsePacket(bytes)
        bytes
    }
  }

  /**
    * Take an indeterminate packet, encode it into something known, and dispatch it.
    * @param pkt the unknown packet
    * @return the byte representation of the packet that was dispatched
    */
  def sendResponse(pkt : PlanetSideGamePacket) : ByteVector = {
    log.trace("CRYPTO SEND GAME: " + pkt)
    val pktEncoded = PacketCoding.EncodePacket(pkt)
    pktEncoded match {
      case Failure(_) =>
        log.error(s"Failed to encode packet ${pkt.getClass.getName} when sending response")
        ByteVector.empty

      case Successful(v) =>
        val bytes = v.toByteVector
        MDC("sessionId") = sessionId.toString
        leftRef !> ResponsePacket(bytes)
        bytes
    }
  }

  /**
    * Generate a random pattern of bytes.
    * @param amount the number of bytes requested
    * @return a `ByteVector` containing the pattern
    */
  def getRandBytes(amount : Int) : ByteVector = {
    val array = Array.ofDim[Byte](amount)
    random.nextBytes(array)
    ByteVector.view(array)
  }
}
