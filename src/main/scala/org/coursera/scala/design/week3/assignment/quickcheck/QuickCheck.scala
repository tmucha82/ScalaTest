package org.coursera.scala.design.week3.assignment.quickcheck

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Properties}


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for {
      element <- Arbitrary.arbitrary[A]
      heap <- Gen.oneOf(Gen.const(empty), genHeap)
    } yield insert(element, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("adding min to heap one more time") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

}
