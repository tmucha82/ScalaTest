package com.sdc.scala.parser

import org.scalatest.FunSuite

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

class ParserTest extends FunSuite {

  object Arith extends JavaTokenParsers {
    def expr: Parser[Any] = term ~ rep("+" ~ term | "-" ~ term)

    def term: Parser[Any] = factor ~ rep("*" ~ factor | "/" ~ factor)

    def factor: Parser[Any] = floatingPointNumber | "(" ~ expr ~ ")"
  }

  object MyParsers extends RegexParsers {
    val ident: Parser[String] = """[a-zA-Z_]\w*""".r
  }

  test("simple arith parsing") {
    assert("[1.12] parsed: ((2~List((*~(((~((3~List())~List((+~(7~List())))))~)))))~List())" === Arith.parseAll(Arith.expr, "2 * (3 + 7)").toString)
    assert("[1.12] failure: `-' expected but `)' found\n\n2 * (3 + 7))\n           ^" === Arith.parseAll(Arith.expr, "2 * (3 + 7))").toString)
  }

  test("regexp parser") {
    assert("[1.11] parsed: helloWorld" === MyParsers.parseAll(MyParsers.ident, "helloWorld").toString)
    assert("[1.1] failure: string matching regex `[a-zA-Z_]\\w*' expected but `^' found\n\n^temp\n^" === MyParsers.parseAll(MyParsers.ident, "^temp").toString)
  }
}
