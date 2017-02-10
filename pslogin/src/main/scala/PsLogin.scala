// Copyright (c) 2016 PSForever.net to present
import java.io.File

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.joran.spi.JoranException
import ch.qos.logback.core.status.{Status, StatusUtil}
import ch.qos.logback.core.util.StatusPrinter
import org.fusesource.jansi.Ansi._
import org.fusesource.jansi.Ansi.Color._
import org.slf4j

object PsLogin {
  private val logger = org.log4s.getLogger

  var args : Array[String] = Array()
  val login : LoginServer.type = LoginServer
  val game : GameServer.type = GameServer

  /**
    * Pretty terminal graphics.
    */
  def banner() : Unit = {
    println(ansi().fgBright(BLUE).a(   """   ___  ________"""))
    println(ansi().fgBright(BLUE).a(   """  / _ \/ __/ __/__  _______ _  _____ ____"""))
    println(ansi().fgBright(MAGENTA).a(""" / ___/\ \/ _// _ \/ __/ -_) |/ / -_) __/"""))
    println(ansi().fgBright(RED).a(    """/_/  /___/_/  \___/_/  \__/|___/\__/_/""").reset())
    println(                           """           PSForever Project""")
    println(                           """          http://psforever.net""")
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
    * An entry point to the combined application launcher.
    * Calls the secondary entry points for the LoginServer and the GameServer code.
    * Also, configures the logger for them, since they're all sharing it.
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

    logger.info("Starting login server ...") //TODO what's going on here?
    login.setup(this.args)
    logger.info("Starting game world ...")
    game.setup(this.args)

    //add our shutdown hook (this works for Control+C as well, but not in Cygwin)
    sys addShutdownHook {
      //TODO: clean up active sessions and close resources safely?
    }
  }

  def main(args : Array[String]) : Unit = {
    this.args = args
    run()

    //run forever, until the servers' ActorSystems shut down
  }
}
