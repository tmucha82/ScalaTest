package com.sdc.scala.stateful

abstract class BasicCircuitSimulation extends Simulation {
  def InverterDelay: Int

  def AndGateDelay: Int

  def OrGateDelay: Int

  class Wire {

    private var signalValue = false
    private var actions: List[Action] = List()

    def getSignal = signalValue

    def setSignal(signal: Boolean) =
      if (signal != signalValue) {
        signalValue = signal
        actions foreach (_())
        //actions foreach (f => f()) /the same as above
      }

    def addAction(action: Action) = {
      actions = action :: actions
      action()
    }
  }

  def inverter(input: Wire, output: Wire) = {
    def invertAction() {
      val inputSignal = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal !inputSignal //clue of inverter
      }
    }
    input addAction invertAction
  }

  def andGate(a: Wire, b: Wire, output: Wire) = {
    def andAction() = {
      val aSignal = a.getSignal
      val bSignal = b.getSignal
      afterDelay(AndGateDelay) {
        output setSignal (aSignal & bSignal) // clue of and cir
      }
    }
    a addAction andAction
    b addAction andAction
  }

  def orGate(a: Wire, b: Wire, output: Wire) {
    def orAction() {
      val aSignal = a.getSignal
      val bSignal = b.getSignal
      afterDelay(OrGateDelay) {
        output setSignal (aSignal | bSignal) // clue of or cir
      }
    }
    a addAction orAction
    b addAction orAction
  }

  def probe(name: String, wire: Wire) {
    def probeAction() {
      println(name + " " + currentTime + " new-value = " + wire.getSignal)
    }
    wire addAction probeAction
  }
}
