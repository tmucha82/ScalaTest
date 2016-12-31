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

  /**
    * If you insert to heap element which is equals to min, then min element of
    * nee heap should be also the same.
    */
  property("adding min to heap one more time") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /**
    * If you insert to empty heap element, it must be min element
    */
  property("adding one element to empty heap must be min") = forAll { (a: A) =>
    findMin(insert(a, empty)) == a
  }

  /**
    * If you insert any two elements into an empty heap, finding the minimum of the resulting heap
    * should get the smallest of the two elements back.
    */
  property("adding two elements to heap") = forAll { (a: A, b: A) =>
    findMin(insert(a, insert(b, empty))) == a.min(b)
  }

  /**
    * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
    */
  property("inserting and deleting min from empty heap") = forAll { (a: A) =>
    deleteMin(insert(a, empty)) == empty
  }

  /**
    * Given any heap, you should get a sorted sequence of elements
    * when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
    */
  property("a sorted sequence of elements (find and delete min)") = forAll { (h: H) =>
    def collectMin(heap: H, listOfMin: List[A]): List[A] = {
      if (isEmpty(heap)) listOfMin
      else collectMin(deleteMin(heap), findMin(heap) :: listOfMin)
    }

    val listOfMin = collectMin(h, Nil)
    listOfMin == listOfMin.sorted.reverse
  }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
  //TODO
//  property("finding a minimum of the melding of any two heaps") = ???
}
