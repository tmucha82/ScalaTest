package com.sdc.scala.actors

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ActorsTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {

  class SillyActor extends Actor {
    override def receive = {
      case _ =>
        for (i <- 1 to 5) {
          println("I'm acting!")
          Thread.sleep(1000)
        }
    }
  }

  class SeriousActor extends Actor {

    override def receive = {
      case _ =>
        for (i <- 1 to 5) {
          println("That is the question.")
          Thread.sleep(1000)
        }
    }
  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "An Echo actor" must {

    "send back messages unchanged" in {
      val actor1 = system.actorOf(Props(classOf[SillyActor], this), name = "sillyActor")
      val actor2 = system.actorOf(Props(classOf[SeriousActor], this), name = "seriousActor")
      actor1 ! "hello world"
      actor2 ! "hello world"
    }

  }
}
