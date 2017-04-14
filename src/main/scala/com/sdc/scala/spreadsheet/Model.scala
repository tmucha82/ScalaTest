package com.sdc.scala.spreadsheet

import scala.swing.event._
import scala.swing.{Publisher, TextField}


class Model(val height: Int, val width: Int) extends Evaluator with Arithmetic {

  case class Cell(row: Int, column: Int) extends TextField with Publisher {
    var f: Formula = Empty
    private var v: Double = 0

    def value: Double = v

    def value_=(w: Double) {
      if (!(v == w || v.isNaN && w.isNaN)) {
        v = w
        publish(new ValueChanged(this))
      }
    }

    def formula: Formula = f

    def formula_=(f: Formula) {
      for (c <- references(formula)) deafTo(c)
      this.f = f
      for (c <- references(formula)) listenTo(c)
      value = evaluate(f)
    }

    override def toString = formula match {
      case Textual(s) => s
      case _ => value.toString
    }
  }

  val cells: Array[Array[Cell]] = (for {
    i <- 0 until height
    j <- 0 until width
  } yield (i, j)).toArray.map { case (x, y) => Cell(x, y) }.grouped(width).toArray
}