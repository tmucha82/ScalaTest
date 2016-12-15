package org.coursera.scala.functional.week4

trait Expr {
}

case class Number(n: Int) extends Expr
case class Var(n: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

object Expr {
  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Var(n) => n.toString
    case Sum(e1, e2) => "(" + show(e1) + " + " + show(e2) + ")"
    case Prod(e1, e2) => show(e1) + " * " + show(e2)
  }
}
