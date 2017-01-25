// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors

/**
  * A simple `object` to retrieve and maintain information about our own network loopback.
  */
object LoginConfig {
  var serverIpAddress : java.net.InetAddress = java.net.InetAddress.getLoopbackAddress
}
