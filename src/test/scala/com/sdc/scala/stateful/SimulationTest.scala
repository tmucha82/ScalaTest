package com.sdc.scala.stateful

import com.sdc.scala.stateful.MySimulation._
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

  test("Circuit simulation") {
    val input1, input2, sum, carry = new Wire
    probe("sum", sum)
    probe("carry", carry)
    halfAdder(input1, input2, sum, carry)
    input1.setSignal(true)
    MySimulation.run()
    input2.setSignal(true)
    MySimulation.run()
  }
}
