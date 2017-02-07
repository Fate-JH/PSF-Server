// Copyright (c) 2016 PSForever.net to present
import org.fusesource.jansi.Ansi._
import org.fusesource.jansi.Ansi.Color._

object PsLogin {
  private val logger = org.log4s.getLogger

  var args : Array[String] = Array()
  val login : LoginServer.type = LoginServer
  val game : GameServer.type = GameServer

  def banner() : Unit = {
    println(ansi().fgBright(BLUE).a(   """   ___  ________"""))
    println(ansi().fgBright(BLUE).a(   """  / _ \/ __/ __/__  _______ _  _____ ____"""))
    println(ansi().fgBright(MAGENTA).a(""" / ___/\ \/ _// _ \/ __/ -_) |/ / -_) __/"""))
    println(ansi().fgBright(RED).a(    """/_/  /___/_/  \___/_/  \__/|___/\__/_/""").reset())
    println(                           """           PSForever Project""")
    println(                           """          http://psforever.net""")
    println
  }

  /** Grabs the most essential system information and returns it as a preformatted string */
  def systemInformation : String = {
    s"""|~~~ System Information ~~~
       |${System.getProperty("os.name")} (v. ${System.getProperty("os.version")}, ${System.getProperty("os.arch")})
       |${System.getProperty("java.vm.name")} (build ${System.getProperty("java.version")}), ${System.getProperty("java.vendor")} - ${System.getProperty("java.vendor.url")}
    """.stripMargin
  }

  /** Used to enumerate all of the Java properties. Used in testing only */
  def enumerateAllProperties() : Unit = {
    val props = System.getProperties
    val enums = props.propertyNames()

    while(enums.hasMoreElements) {
      val key = enums.nextElement.toString
      System.out.println(key + " : " + props.getProperty(key))
    }
  }

  def run() : Unit = {
    // Early start up
    banner()
    println(systemInformation)

    logger.info("Starting login server ...") //TODO what's going on here?
    login.setup(this.args)
    logger.info("Starting game world ...")
    game.setup(this.args)

    // Add our shutdown hook (this works for Control+C as well, but not in Cygwin)
    sys addShutdownHook {
      // TODO: clean up active sessions and close resources safely?
    }
  }

  def main(args : Array[String]) : Unit = {
    this.args = args
    run()

    //run forever, until the servers' ActorSystems shut down
  }
}
