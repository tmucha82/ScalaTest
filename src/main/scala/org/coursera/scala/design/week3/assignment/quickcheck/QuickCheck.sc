import org.coursera.scala.design.week3.assignment.quickcheck.{IntHeap, BinomialHeap}
import org.scalacheck.{Arbitrary, Gen}

lazy val genMap: Gen[Map[Int,Int]] = for {
  k <- Arbitrary.arbitrary[Int]
  v <- Arbitrary.arbitrary[Int]
  m <- Gen.oneOf(Gen.const(Map.empty[Int,Int]), genMap)
} yield m.updated(k, v)
Arbitrary.arbitrary[Int].sample
//def empty: H // the empty heap
//def isEmpty(h: H): Boolean // whether the given heap h is empty
//
//def insert(x: A, h: H): H // the heap resulting from inserting x into h
//def meld(h1: H, h2: H): H // the heap resulting from merging h1 and h2
//
//def findMin(h: H): A // a minimum of the heap h
//def deleteMin(h: H): H // a heap resulting from deleting a minimum of h
class MyIntHeap extends BinomialHeap with IntHeap {
  lazy val genHeap: Gen[H] = {
    for {
      element <- Arbitrary.arbitrary[Int]
      heap <- Gen.oneOf(Gen.const(empty), genHeap)
    } yield insert(element, heap)
  }
  def collectMin(heap: H, listOfMin: List[A]): List[A] = {
    if (isEmpty(heap)) listOfMin
    else collectMin(deleteMin(heap), findMin(heap) :: listOfMin)
  }
}
val heapManager = new MyIntHeap
heapManager.genHeap.sample
heapManager.genHeap.sample
heapManager.genHeap.sample
heapManager.genHeap.sample
heapManager.empty
heapManager.isEmpty(heapManager.empty)
// 1, 3, 5, 8, -1
var testHeap = heapManager.insert(1, heapManager.insert(3, heapManager.insert(5, heapManager.insert(8, heapManager.insert(-1, heapManager.empty)))))
heapManager.collectMin(testHeap, Nil)
// -1
heapManager.findMin(testHeap)
testHeap = heapManager.deleteMin(testHeap)
// 1
heapManager.findMin(testHeap)
testHeap = heapManager.deleteMin(testHeap)
// 3
heapManager.findMin(testHeap)
testHeap = heapManager.deleteMin(testHeap)
// 5
heapManager.findMin(testHeap)
testHeap = heapManager.deleteMin(testHeap)
// 8
heapManager.findMin(testHeap)
testHeap = heapManager.deleteMin(testHeap)
heapManager.meld(testHeap, heapManager.empty)




