package org.coursera.scala.design.week4

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

/**
  * Created by Tomek on 2017-01-05.
  */
object CoffeeFuture extends App {

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.util.Random

  type CoffeeBeans = String
  type GroundCoffee = String

  case class Water(temperature: Int)

  type Milk = String
  type FrothedMilk = String
  type Espresso = String
  type Cappuccino = String

  case class GrindingException(msg: String) extends Exception(msg)

  case class FrothingException(msg: String) extends Exception(msg)

  case class WaterBoilingException(msg: String) extends Exception(msg)

  case class BrewingException(msg: String) extends Exception(msg)

  def grind(beans: CoffeeBeans): Future[GroundCoffee] = Future {
    println("start grinding...")
    Thread.sleep(Random.nextInt(2000))
    if (beans == "baked beans") throw GrindingException("are you joking?")
    println("finished grinding...")
    s"ground coffee of $beans"
  }

  def heatWater(water: Water): Future[Water] = Future {
    println("heating the water now")
    Thread.sleep(Random.nextInt(2000))
    println("hot, it's hot!")
    water.copy(temperature = 85)
  }

  def frothMilk(milk: Milk): Future[FrothedMilk] = Future {
    println("milk frothing system engaged!")
    Thread.sleep(Random.nextInt(2000))
    println("shutting down milk frothing system")
    s"frothed $milk"
  }

  def brew(coffee: GroundCoffee, heatedWater: Water): Future[Espresso] = Future {
    println("happy brewing :)")
    Thread.sleep(Random.nextInt(2000))
    println("it's brewed!")
    "espresso"
  }

  def combine(espresso: Espresso, frothedMilk: FrothedMilk): Cappuccino = "cappuccino"

  /**
    * you can use on success or ...
    */
  grind("arabica beans").onSuccess { case ground =>
    println("okay, got my ground coffee")
  }

  /**
    * you can use on success or ...
    */
  grind("baked beans").onComplete {
    case Success(ground) => println(s"got my $ground")
    case Failure(ex) => println("This grinder needs a replacement, seriously!")
  }


  val temperatureOkay: Future[Boolean] = heatWater(Water(25)).map { water =>
    println("we're in the future!")
    (80 to 85).contains(water.temperature)
  }
  temperatureOkay.onComplete {
    case Success(value) => println("water" + (if (value) "is" else "is not") + " hot enough")
    case Failure(ex) => println("Problem with heating water")
  }


  def isTemperatureOkay(water: Water): Future[Boolean] = Future {
    (80 to 85).contains(water.temperature)
  }

  //  Use flatMap instead of map in order to get a Future[Boolean] instead of a Future[Future[Boolean]]:
  val nestedFuture: Future[Future[Boolean]] = heatWater(Water(25)).map {
    water => isTemperatureOkay(water)
  }
  val flatFuture: Future[Boolean] = heatWater(Water(25)).flatMap {
    water => isTemperatureOkay(water)
  }

  def prepareCappuccino(): Future[Cappuccino] = {
    val groundCoffee = grind("arabica beans")
    val heatedWater = heatWater(Water(20))
    val frothedMilk = frothMilk("milk")
    for {
      ground <- groundCoffee
      water <- heatedWater
      foam <- frothedMilk
      espresso <- brew(ground, water)
    } yield combine(espresso, foam)
  }


  val prepareCappuccinoFuture = prepareCappuccino()
  prepareCappuccinoFuture.onComplete{
    case Success(cappuccino) => println("Cappuccino " + cappuccino + " is ready")
    case Failure(ex) => println("Problem with creating cappuccino")
  }

  Await.result(prepareCappuccinoFuture, Duration.Inf)
}
