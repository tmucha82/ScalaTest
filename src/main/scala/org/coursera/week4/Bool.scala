package org.coursera.week4

abstract class Bool {
  def ifThanElse[T](t: => T, e: => T): T

  def &&(x: => Bool): Bool = ifThanElse(x, False)

  def ||(x: => Bool): Bool = ifThanElse(True, x)

  def unary_! : Bool = ifThanElse(False, True)

  def ==(x: => Bool): Bool = ifThanElse(x, !x)

  def !=(x: => Bool): Bool = ifThanElse(!x, x)

  def <(x: => Bool): Bool = ifThanElse(False, x)

}

object False extends Bool {
  override def ifThanElse[T](t: => T, e: => T): T = e
}

object True extends Bool {
  override def ifThanElse[T](t: => T, e: => T): T = t
}




