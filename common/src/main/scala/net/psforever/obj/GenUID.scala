// Copyright (c) 2016 PSForever.net to present
package net.psforever.obj

trait GenUID {
  @throws(classOf[Exception])
  def release : Int
  @throws(classOf[Exception])
  def release(num : Int) : Array[Int]
  def recover() : Boolean
  def recover(d : Int) : Boolean
}
