package org.coursera.week3.assignment

import org.coursera.week3.assignment.objsets.{Empty, Tweet, TweetSet}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {

  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6 = set5.incl(new Tweet("g", "g body", 112))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("foreach: on set6 println") {
    new TestSets {
      set6.foreach(println(_))
    }
  }

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("mostRetweeted: on set1") {
    new TestSets {
      assert(set1.mostRetweeted === null)
    }
  }

  test("mostRetweeted: on set2") {
    new TestSets {
      assert(set2.mostRetweeted.retweets === 20)
    }
  }

  test("mostRetweeted: on set3") {
    new TestSets {
      assert(set3.mostRetweeted.retweets === 20)
    }
  }

  test("mostRetweeted: on set4c") {
    new TestSets {
      assert(set4c.mostRetweeted.retweets === 20)
    }
  }

  test("mostRetweeted: on set4d") {
    new TestSets {
      assert(set4d.mostRetweeted.retweets === 20)
    }
  }

  test("mostRetweeted on set5") {
    new TestSets {
      assert(set5.mostRetweeted.retweets === 20)
    }
  }

  test("mostRetweeted on set6") {
    new TestSets {
      assert(set6.mostRetweeted.retweets === 112)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

}
