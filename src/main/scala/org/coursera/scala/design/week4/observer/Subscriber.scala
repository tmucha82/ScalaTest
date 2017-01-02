package org.coursera.scala.design.week4.observer

trait Subscriber {
  def handle(publisher: Publisher)
}
