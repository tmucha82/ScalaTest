package com.sdc.scala.parser

import org.scalatest.FunSuite

import scala.util.parsing.combinator.RegexParsers

class ParserTest extends FunSuite {

  object MyParsers extends RegexParsers {
    val ident: Parser[String] = """[a-zA-Z_]\w*""".r
  }

  trait TestSet {
    val arith = new Arith()

    val example1 = "2 * (3 + 7)"
    val failureExample = "2 * (3 + 7))"
  }

  test("simple arith parsing") {
    new TestSet {
      assert("[1.12] parsed: ((2~List((*~(((~((3~List())~List((+~(7~List())))))~)))))~List())" === arith.parseAll(arith.expr, example1).toString)
      assert("[1.12] failure: `-' expected but `)' found\n\n2 * (3 + 7))\n           ^" === arith.parseAll(arith.expr, failureExample).toString)
    }
  }

  test("regexp parser") {
    assert("[1.11] parsed: helloWorld" === MyParsers.parseAll(MyParsers.ident, "helloWorld").toString)
    assert("[1.1] failure: string matching regex `[a-zA-Z_]\\w*' expected but `^' found\n\n^temp\n^" === MyParsers.parseAll(MyParsers.ident, "^temp").toString)
  }
}
