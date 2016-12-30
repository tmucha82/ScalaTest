package com.sdc.scala.stateful

abstract class Simulation {

  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private var time = 0

  def currentTime: Int = time

  private var agenda: List[Event] = List()

  private def insert(events: List[Event], item: Event): List[Event] = {
    if (events.isEmpty || item.time < events.head.time) item :: events
    else events.head :: insert(events.tail, item)
  }

  def afterDelay(delay: Int)(block: => Unit) {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def next() {
    (agenda: @unchecked) match {
      case event :: rest =>
        agenda = rest
        time = event.time
        event.action()
    }
  }

  def run() {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }
    while (agenda.nonEmpty) next()
  }
}