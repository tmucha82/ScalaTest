package com.sdc.scala.extractor

import org.scalatest.FunSuite

class ExtractorTest extends FunSuite {

  test("extracting email addresses - method #1") {
    /**
     * You could do that standard way:
     * def isEMail(s: String): Boolean
     * def domain(s: String): String
     * def user(s: String): String
     *
     * and
     * if (isEMail(s)) println(user(s) +" AT "+ domain(s))
     * else println("not an email address")
     *
     * checking if the mail are equal ar also this way...but... what about pattern matching and case class
     */

    case class EMail(user: String, domain: String)

    //then look how easy it is
    def toString(email: Any): String = {
      email match {
        case EMail(user, domain) => user + " AT " + domain
        case _ => "not an email address"
      }
    }
    assert("tmucha AT sdc.dk" === toString(EMail("tmucha", "sdc.dk")))
    assert("not an email address" === toString("tmucha"))

    def startsWithTwoSameEmails(emails: List[EMail]): Boolean = {
      emails match {
        case EMail(u1, d1) :: EMail(u2, d2) :: _ if u1 == u2 => true
        case _ => false
      }
    }

    assert(startsWithTwoSameEmails(List(EMail("tmucha", "sdc.dk"), EMail("tmucha", "gmail.com"))))
    assert(startsWithTwoSameEmails(List(EMail("tmucha", "sdc.dk"), EMail("tmucha", "gmail.com"), EMail("mkrupa", "sdc.dk"))))
    assert(!startsWithTwoSameEmails(List(EMail("tmucha", "sdc.dk"), EMail("mkrupa", "sdc.dk"), EMail("tmucha", "gmail.com"))))
  }

  test("extracting email addresses - method #2") {
    //now extractor
    object EMail {

      // The injection method (optional)
      def apply(user: String, domain: String) = user + "@" + domain

      // The extraction method (mandatory)
      def unapply(str: String): Option[(String, String)] = {
        val parts = str split "@"
        if (parts.length == 2) Some(parts(0), parts(1)) else None
      }
    }

    assert("tmucha@gmail.com" === EMail("tmucha", "gmail.com"))
    assert(EMail.unapply("John@epfl.ch") === Some("John", "epfl.ch"))
    assert(EMail.unapply("John Doe") === None)

    //now we can do
    assert(EMail("tmucha", "gmail.com") match {
      case EMail(user, domain) => true
      case _ => false
    })

    //...and even that
    assert("tmucha@gmail.com" match {
      //because of unapply
      case EMail(user, domain) => true //because of apply
      case _ => false
    })
    assert(false === ("tmucha.gmail.com" match {
      //because of unapply
      case EMail(user, domain) => true //because of apply
      case _ => false
    }))
  }

  test("patterns with zero or one variables") {
    object EMail {

      // The injection method (optional)
      def apply(user: String, domain: String) = user + "@" + domain

      // The extraction method (mandatory)
      def unapply(str: String): Option[(String, String)] = {
        val parts = str split "@"
        if (parts.length == 2) Some(parts(0), parts(1)) else None
      }
    }

    object Twice {
      def apply(s: String): String = s + s

      def unapply(s: String): Option[String] = {
        val length = s.length / 2
        val half = s.substring(0, length)
        if (half == s.substring(length)) Some(half) else None
      }
    }

    object UpperCase {
      def unapply(s: String): Boolean = s.toUpperCase == s
    }

    assert("tomtom" === Twice("tom"))
    assert("tom" === Twice.unapply("tomtom").get)
    assert(false === Twice.unapply("tomasz").isDefined)

    //wow - tricky
    def userTwiceUpper(mail: String) = mail match {
      case EMail(Twice(x@UpperCase()), domain) => true
      case _ => false
    }

    assert(false === userTwiceUpper("tmucha82@gmail.com"))
    assert(false === userTwiceUpper("Tmucha82@gmail.com"))
    assert(false === userTwiceUpper("tomtom@gmail.com"))
    assert(true === userTwiceUpper("TOMTOM@gmail.com"))
  }

  test("variable argument extractors") {
    object EMail {

      // The injection method (optional)
      def apply(user: String, domain: String) = user + "@" + domain

      // The extraction method (mandatory)
      def unapply(str: String): Option[(String, String)] = {
        val parts = str split "@"
        if (parts.length == 2) Some(parts(0), parts(1)) else None
      }
    }

    object Domain {
      // The injection method (optional)
      def apply(parts: String*): String =
        parts.reverse.mkString(".")

      // The extraction method (mandatory)
      def unapplySeq(whole: String): Option[Seq[String]] =
        Some(whole.split("\\.").reverse)
    }

    assert("gmail.com" === Domain("com", "gmail"))
    assert(Domain.unapplySeq("gmail.com").get === Array("com", "gmail"))


    def isTomInDotCom(s: String): Boolean = s match {
      case EMail("tom", Domain("com", _*)) => true
      case _ => false
    }

    assert(true === isTomInDotCom("tom@sun.com"))
    assert(false === isTomInDotCom("peter@sun.com"))
    assert(false === isTomInDotCom("tom@acm.org"))
  }

  test("expandedEMail extractor object") {
    object ExpandedEMail {
      def unapplySeq(email: String): Option[(String, Seq[String])] = {
        val parts = email split "@"
        if (parts.length == 2) {
          val seq: Seq[String] = parts(1).split("\\.").reverse
          Some((parts(0), seq))
        }
        else
          None
      }
    }
    val s = "tom@support.epfl.ch"
    val ExpandedEMail(name, mainDomain, subDomains@_*) = s
    assert("tom" === name)
    assert("ch" === mainDomain)
    assert(List("epfl", "support") === subDomains)
  }

  test("extractors versus case classes") {
    /**
     * So which of the two methods should you prefer for your pattern matches?
     * It depends. If you write code for a closed application,
     * case classes are usually preferable because of their advantages in conciseness,
     * speed and static checking. If you decide to change your class hierarchy later,
     * the application needs to be refactored, but this is usually not a problem.
     * On the other hand, if you need to expose a type to unknown clients,
     * extractors might be preferable because they maintain representation independence.
     */
  }

  test("regular expressions") {
    /**
     * You cna go 3 ways
     * val decimal = new Regex("(-)?(\\d+)(\\.\\d*)?")
     * val decimal = new Regex("""(-)?(\d+)(\.\d*)?""")
     * val decimal = """(-)?(\d+)(\.\d*)?""".r
     */
    val decimal = """(-)?(\d+)(\.\d*)?""".r

    val input = "for -1.0 to 99 by 3"
    val result = for (s <- (decimal findAllIn input).toList) yield s
    assert(List("-1.0", "99", "3") === result)
    val decimal(sign, integerPart, decimalPart) = "-1.23"
    assert("-" === sign)
    assert("1" === integerPart)
    assert(".23" === decimalPart)

    val decimal(sign2, integerPart2, decimalPart2) = "1.0"
    //sign2 is null
    assert(null.asInstanceOf[String] === sign2)
    assert("1" === integerPart2)
    assert(".0" === decimalPart2)
  }
}
