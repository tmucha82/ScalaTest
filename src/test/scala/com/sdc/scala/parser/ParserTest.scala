package com.sdc.scala.parser

import org.scalatest.FunSuite

import scala.util.parsing.combinator._

class ParserTest extends FunSuite {

  object Arith extends JavaTokenParsers {
    def expr: Parser[Any] = term ~ rep("+" ~ term | "-" ~ term)

    def term: Parser[Any] = factor ~ rep("*" ~ factor | "/" ~ factor)

    def factor: Parser[Any] = floatingPointNumber | "(" ~ expr ~ ")"
  }

  object SimpleJSON extends JavaTokenParsers {

    def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false"

    def obj: Parser[Any] = "{" ~ repsep(member, ",") ~ "}"

    def arr: Parser[Any] = "[" ~ repsep(value, ",") ~ "]"

    def member: Parser[Any] = stringLiteral ~ ":" ~ value
  }

  object ParsedJSON extends JavaTokenParsers {
    def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber ^^ (_.toDouble) | "null" ^^ (x => null) | "true" ^^ (x => true) | "false" ^^ (x => false)

    def obj: Parser[Map[String, Any]] = "{" ~> repsep(member, ",") <~ "}" ^^ (Map() ++ _)

    def arr: Parser[List[Any]] = "[" ~> repsep(value, ",") <~ "]"

    def member: Parser[(String, Any)] = stringLiteral ~ ":" ~ value ^^ { case name ~ ":" ~ value => (name, value) }
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

  test("SimpleJSON parser") {
    val json = "{\"address book\": {\"name\": \"John Smith\",\"address\": {\"street\": \"10 Market Street\",\"city\"  : \"San Francisco, CA\",\"zip\"   : 94111},\"phone numbers\": [\"408 338-4238\",\"408 111-6892\"]}}"
    assert("[1.178] parsed: (({~List(((\"address book\"~:)~(({~List(((\"name\"~:)~\"John Smith\"), ((\"address\"~:)~(({~List(((\"street\"~:)~\"10 Market Street\"), ((\"city\"~:)~\"San Francisco, CA\"), ((\"zip\"~:)~94111)))~})), ((\"phone numbers\"~:)~(([~List(\"408 338-4238\", \"408 111-6892\"))~]))))~}))))~})" === SimpleJSON.parseAll(SimpleJSON.value, json).toString)

    val invalidJson = "{ test:1.1 }"
    assert("[1.3] failure: `}' expected but `t' found\n\n{ test:1.1 }\n  ^" === SimpleJSON.parseAll(SimpleJSON.value, invalidJson).toString)
  }

  test("ParsedJSON parser") {
    val json = "{\"address book\": {\"name\": \"John Smith\",\"address\": {\"street\": \"10 Market Street\",\"city\"  : \"San Francisco, CA\",\"zip\"   : 94111},\"phone numbers\": [\"408 338-4238\",\"408 111-6892\"]}}"

    val parsed = ParsedJSON.parseAll(ParsedJSON.value, json)
    assert(parsed.successful)
    assert(parsed.next.source === json)
    val result: Map[String, Any] = parsed.get.asInstanceOf[Map[String, Any]]
    val only = result.head
    assert("\"address book\"" === only._1)
    assert(("\"name\"", "\"John Smith\"") === only._2.asInstanceOf[Map[String, Any]].head)
  }

}
