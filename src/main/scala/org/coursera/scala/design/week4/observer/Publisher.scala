package org.coursera.scala.design.week4.observer

trait Publisher {

  private var subscribers: Set[Subscriber] = Set.empty

  def subcribe(subscriber: Subscriber) = subscribers += subscriber

  def unsubscribe(subscriber: Subscriber) = subscribers -= subscriber

  def publish() = subscribers.foreach(_.handle(this))
}
