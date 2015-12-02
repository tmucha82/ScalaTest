package com.sdc.scala.stateful

import org.scalatest.FunSuite

class SimulationTest extends FunSuite {

  test("empty simulation") {
    val simulation = new Simulation {}

    def printlnAction = println(_: Int)

    def addWorkItem(delay: Int): Unit = {
      simulation.afterDelay(delay)(printlnAction(delay))
    }

    addWorkItem(1)
    addWorkItem(2)
    simulation.run()
  }
}
