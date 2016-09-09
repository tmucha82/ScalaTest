package com.sdc.scala.junit

import java.util

import com.sdc.scala.types.Wild
import org.junit.{Assert, Test}

class JUnitTest {

  @Test
  def testMultiAdd(): Unit = {
    val set = Set() + 1 + 2 + 3 + 1 + 2 + 3
    Assert.assertEquals(3, set.size)
  }

  abstract class SetAndType {
    type Elem
    val set: Set[Elem]
  }

  def javaSet2ScalaSet[T](javaSet: util.Collection[T]): SetAndType = {
    var scalaSet = Set.empty[T] // now T can be named!

    val iterator = javaSet.iterator
    while (iterator.hasNext)
      scalaSet += iterator.next()

    new SetAndType {
      type Elem = T
      val set = scalaSet
    }
  }

  @Test
  def testExistentialTypes(): Unit = {
    val wild = new Wild
    Assert.assertEquals(3, wild.contents.size)

    val set = javaSet2ScalaSet(wild.contents())
    Assert.assertEquals(Set("a", "b", "see"), set.set)
  }
}
