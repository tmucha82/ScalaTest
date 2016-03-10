package com.sdc.scala.extractor

import org.scalatest.FunSuite

class ExtractorTest extends FunSuite {

  test("extracting email addresses - method #1") {
    /**
     * You could do that standard way:
     *   def isEMail(s: String): Boolean
     *   def domain(s: String): String
     *   def user(s: String): String
     *
     *   and
     *   if (isEMail(s)) println(user(s) +" AT "+ domain(s))
     *   else println("not an email address")
     *
     *   checking if the mail are equal ar also this way...but... what about pattern matching and case class
     */

    case class EMail(user: String, domain: String)

    //then look how easy it is
    def toString(email: Any): String = {
      email match {
        case EMail(user, domain) => user +" AT "+ domain
        case _ => "not an email address"
      }
    }
    assert("tmucha AT sdc.dk" === toString(EMail("tmucha", "sdc.dk")))
    assert("not an email address" === toString("tmucha"))

    def startsWithTwoSameEmails(emails: List[EMail]) : Boolean = {
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
    assert("tmucha@gmail.com" match { //because of unapply
      case EMail(user, domain) => true //because of apply
      case _ => false
    })
    assert(false === ("tmucha.gmail.com" match { //because of unapply
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
      case EMail(Twice(x @ UpperCase()), domain) => true
      case _ => false
    }

    assert(false === userTwiceUpper("tmucha82@gmail.com"))
    assert(false === userTwiceUpper("Tmucha82@gmail.com"))
    assert(false === userTwiceUpper("tomtom@gmail.com"))
    assert(true === userTwiceUpper("TOMTOM@gmail.com"))
  }

}
