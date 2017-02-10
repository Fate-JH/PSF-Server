// Copyright (c) 2016 PSForever.net to present
import java.io.File
import java.net.InetAddress

import actors.LoginSessionActor
import akka.actor.{ActorSystem, Props}
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException
import ch.qos.logback.core.status.{Status, StatusUtil}
import ch.qos.logback.core.util.StatusPrinter
import com.typesafe.config.ConfigFactory
import net.psforever.actors.session.{SessionPipeline, SessionRouter}
import net.psforever.actors.udp.UdpListener
import net.psforever.actors.{CryptoSessionActor, LoopbackConfig}
import net.psforever.crypto.CryptoInterface
import org.fusesource.jansi.Ansi.Color._
import org.fusesource.jansi.Ansi._
import org.slf4j

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object LoginServer {
  private val logger = org.log4s.getLogger

  var args : Array[String] = Array()
  var config : java.util.Map[String,Object] = null
  var system : akka.actor.ActorSystem = null
  var loginRouter : akka.actor.Props = null
  var loginListener : akka.actor.ActorRef = null

  /**
    * Pretty terminal graphics.
    */
  def banner() : Unit = {
    println(ansi().fgBright(BLUE).a(   """   ___  ________"""))
    println(ansi().fgBright(BLUE).a(   """  / _ \/ __/ __/__  _______ _  _____ ____"""))
    println(ansi().fgBright(MAGENTA).a(""" / ___/\ \/ _// _ \/ __/ -_) |/ / -_) __/"""))
    println(ansi().fgBright(RED).a(    """/_/  /___/_/  \___/_/  \__/|___/\__/_/""").reset())
    println(                           """   Login Server - PSForever Project""")
    println(                           """         http://psforever.net""")
    println
  }

  /**
    * Grabs essential system information and returns it as a formatted string.
    * @return the system information
    */
  def systemInformation : String = {
    s"""|~~~ System Information ~~~
        |${System.getProperty("os.name")} (v. ${System.getProperty("os.version")}, ${System.getProperty("os.arch")})
        |${System.getProperty("java.vm.name")} (build ${System.getProperty("java.version")}), ${System.getProperty("java.vendor")} - ${System.getProperty("java.vendor.url")}
    """.stripMargin
  }

  /**
    * Enumerate all of the Java properties.
    * Testing only.
    */
  def enumerateAllProperties() : Unit = {
    val props = System.getProperties
    val enums = props.propertyNames()

    while(enums.hasMoreElements) {
      val key = enums.nextElement.toString
      System.out.println(key + " : " + props.getProperty(key))
    }
  }

  /**
    * Checks the current logger context
    * @param context SLF4J logger context
    * @return Boolean return true if context has errors
    */
  def loggerHasErrors(context : LoggerContext) : Boolean = {
    val statusUtil = new StatusUtil(context)

    statusUtil.getHighestLevel(0) >= Status.WARN
  }

  /**
    * Loads the logging configuration and starts logging
    * @param logfile the file name where information for this application will be saved
    */
  def initializeLogging(logfile : String) : Unit = {
    //assume SLF4J is bound to logback in the current environment
    val lc = slf4j.LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    try {
      val configurator = new JoranConfigurator()
      configurator.setContext(lc)
      lc.reset() //reset any loaded settings
      configurator.doConfigure(logfile)
    }
    catch {
      case je : JoranException =>
    }

    if(loggerHasErrors(lc)) {
      println("Loading log settings failed")
      StatusPrinter.printInCaseOfErrorsOrWarnings(lc)
      sys.exit(1)
    }
  }

  /**
    * If we've inherited any command line parameters, dissect them and apply them.
    * @param args the arguments in list form
    */
  def parseArgs(args : Array[String]) : Unit = {
    if(args.length == 1) {
      LoopbackConfig.serverIpAddress = InetAddress.getByName(args{0})
    }
    else {
      LoopbackConfig.serverIpAddress = InetAddress.getLocalHost
    }
  }

  /**
    * An entry point to the login application.
    * Calls the secondary entry point for this `LoginServer`.
    * This entry point performs basic reporting that is displayed when the `LoginServer` is operating on its own.
    */
  def run() : Unit = {
    //early start up
    banner()
    println(systemInformation)

    //config directory; assume a default of the current directory
    var configDirectory = "config"
    //defined when we are running from SBT pack
    if(System.getProperty("prog.home") != null) {
      configDirectory = System.getProperty("prog.home") + File.separator + "config"
    }
    initializeLogging(configDirectory + File.separator + "logback.xml")
    logger.info(s"Detected ${Runtime.getRuntime.availableProcessors()} available logical processor(s)")
    setup(Array.empty)
  }

  /**
    * A secondary entry point to the game world application.
    * This entry point skips over basic reporting that is displayed when the `LoginServer` is operating on its own.
    * @param args the arguments in list form
    */
  def setup(args : Array[String]) : Unit = {
    parseArgs(args ++ this.args)
    /**
      * Initialize the PSCrypto native library
      *
      * PSCrypto provides PlanetSide specific crypto that is required to communicate with it.
      * It has to be distributed as a native library because there is no Scala version of the required
      * cryptographic primitives (MD5MAC). See https://github.com/psforever/PSCrypto for more information.
      */
    try {
      CryptoInterface.initialize()
      logger.info("PSCrypto initialized")
    }
    catch {
      case e : UnsatisfiedLinkError =>
        logger.error("Unable to initialize " + CryptoInterface.libName)
        logger.error(e)("This means that your PSCrypto version is out of date. Get the latest version from the README" +
          " https://github.com/psforever/PSF-LoginServer#downloading-pscrypto")
        sys.exit(1)
      case e : IllegalArgumentException =>
        logger.error("Unable to initialize " + CryptoInterface.libName)
        logger.error(e)("This means that your PSCrypto version is out of date. Get the latest version from the README" +
          " https://github.com/psforever/PSF-LoginServer#downloading-pscrypto")
        sys.exit(1)
    }

    logger.info("Starting actor subsystems...")
    /**
      * Make sure we capture Akka messages (but only INFO and above)
      *
      * This same config can be specified in a configuration file, but that's more work at this point.
      * In the future we will have a unified configuration file specific to this server
      */
    config = Map(
      "akka.loggers" -> List("akka.event.slf4j.Slf4jLogger").asJava,
      "akka.loglevel" -> "INFO",
      "akka.logging-filter" -> "akka.event.slf4j.Slf4jLoggingFilter"
    ).asJava

    /** Start up the main actor system. This "system" is the home for all actors running on this server */
    system = ActorSystem("PS_LoginServer", ConfigFactory.parseMap(config))

    /** Create a pipeline for the login server
      *
      * The first node in the pipe is an Actor that handles the crypto for protecting packets.
      * After any crypto operations have been applied or unapplied, the packets are passed on to the next
      * actor in the chain. For an incoming packet, this is a player session handler. For an outgoing packet
      * this is the session router, which returns the packet to the sending host.
      *
      * Login sessions are divided between two actors. The crypto session actor transparently handles all of the cryptographic
      * setup of the connection. Once a correct crypto session has been established, all packets, after being decrypted
      * will be passed on to the login session actor. This actor has important state that is used to maintain the login
      * session.
      *
      *                      > PlanetSide net.psforever.actors.Session Pipeline <
      *
      *            read()                  route                decrypt
      * UDP Socket -----> [Session Router] -----> [Crypto Actor] -----> [Session Actor]
      *     /|\             |         /|\          |       /|\                |
      *      |     write()  |          |  encrypt  |        |   response      |
      *      +--------------+          +-----------+        +-----------------+
      **/
    val loginTemplate = List(
      SessionPipeline("crypto-session-", Props[CryptoSessionActor]),
      SessionPipeline("login-session-", Props[LoginSessionActor])
    )

    val loginServerPort = 51000

    // Uncomment for network simulation
    // TODO: make this config or command flag
    /*
    val netParams = net.psforever.actors.udp.NetworkSimulatorParameters(
      packetLoss = 0.02,
      packetDelay = 500,
      packetReorderingChance = 0.005,
      packetReorderingTime = 400
    )
    */

    loginRouter = Props(new SessionRouter("Login", loginTemplate))
    loginListener = system.actorOf(Props(new UdpListener(loginRouter, "login-session-router", LoopbackConfig.serverIpAddress, loginServerPort)), "login-udp-endpoint")

    logger.info(s"NOTE: Set client.ini to point to ${LoopbackConfig.serverIpAddress.getHostAddress}:$loginServerPort")

    //add our shutdown hook (this works for Control+C as well, but not in Cygwin)
    sys addShutdownHook {
      //TODO: clean up active sessions and close resources safely
      logger.info("Login server now shutting down...")
      system.terminate
    }
  }

  def main(args : Array[String]) : Unit = {
    this.args = args
    run()

    //wait forever until the actor system shuts down
    Await.result(system.whenTerminated, Duration.Inf)
  }
}
