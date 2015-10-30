package com.sdc.scala.expressions

import com.sdc.scala.element.Element

class ExpressionFormatter {

  // Contains operators in groups of increasing precedence
  private var operatorGroups = Array(
    Set("|", "||"),
    Set("&", "&&"),
    Set("^"),
    Set("==", "!="),
    Set("<", "<=", ">", ">="),
    Set("+", "-"),
    Set("*", "%")
  )

  private val unaryPrecedence = operatorGroups.length
  private val fractionPrecedence = -1

  // A mapping from operators to their precedence
  private val precedence = {
    val associations =
      for {
        i <- 0 until operatorGroups.length
        operator <- operatorGroups(i)
      } yield operator -> i
    Map() ++ associations
  }

  private def format(expression: Expression, enclPrecedence: Int): Element =

    expression match {

      case Variable(name) => Element.create(name)

      case Number(num) =>
        def stripDot(s: String) =
          if (s endsWith ".0") s.substring(0, s.length - 2)
          else s
        Element.create(stripDot(num.toString))

      case UnaryOperator(op, arg) =>
        Element.create(op) beside format(arg, unaryPrecedence)

      case BinaryOperator("/", left, right) =>
        val top = format(left, fractionPrecedence)
        val bot = format(right, fractionPrecedence)
        val line = Element.create('-', top.width max bot.width, 1)
        val fraction = top above line above bot
        if (enclPrecedence != fractionPrecedence) fraction
        else Element.create(" ") beside fraction beside Element.create(" ")

      case BinaryOperator(op, left, right) =>
        val operatorPrecedence = precedence(op)
        val l = format(left, operatorPrecedence)
        val r = format(right, operatorPrecedence + 1)
        val operator = l beside Element.create(" " + op + " ") beside r
        if (enclPrecedence <= operatorPrecedence) operator
        else Element.create("(") beside operator beside Element.create(")")
    }

  def format(expression: Expression): Element = format(expression, 0)
}
