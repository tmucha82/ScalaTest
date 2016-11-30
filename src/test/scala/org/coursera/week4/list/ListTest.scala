package org.coursera.week4.list

import org.scalatest.FunSuite

import scala.collection.immutable.List


class ListTest extends FunSuite {

  def sort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case p :: ps => insert(p, sort(ps))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if(x <= y) x :: xs else y :: insert(x, ys)
  }

  test("insertion test") {
    val list = List(7, 9, 5, 2)
    println(sort(list))
  }
}
