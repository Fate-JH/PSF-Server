// Copyright (c) 2016 PSForever.net to present
package net.psforever.actors.udp

/** Parameters for the Network simulator
  *
  * @param packetLoss The percentage from [0.0, 1.0] that a packet will be lost
  * @param packetDelay The end-to-end delay (ping) of all packets
  * @param packetReorderingChance The percentage from [0.0, 1.0] that a packet will be reordered
  * @param packetReorderingTime The absolute adjustment in milliseconds that a packet can have (either
  *                             forward or backwards in time)
  */
case class NetworkSimulatorParameters(packetLoss : Double,
                                      packetDelay : Int,
                                      packetReorderingChance : Double,
                                      packetReorderingTime : Int) {
  assert(packetLoss >= 0.0 && packetLoss <= 1.0)
  assert(packetDelay >= 0)
  assert(packetReorderingChance >= 0.0 && packetReorderingChance <= 1.0)
  assert(packetReorderingTime >= 0)
}
