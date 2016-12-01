package org.coursera.week4.assignment.patmat

import org.coursera.week4.assignment.patmat.Huffman.{Fork, Leaf}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(Huffman.weight(t1) === 5)
      assert(Huffman.weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(Huffman.chars(t1) === List('a', 'b'))
      assert(Huffman.chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(Huffman.string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    new TestTrees {
      assert(List(('a', 2), ('b', 1)) === Huffman.times(List('a', 'b', 'a')))
      assert(List(('a', 3), ('b', 1), ('c', 2)) === Huffman.times(List('a', 'b', 'c', 'a', 'c', 'a')))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(Huffman.makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
    assert(Huffman.makeOrderedLeafList(List(('a', 2), ('b', 1))) === List(Leaf('b', 1), Leaf('a', 2)))
    assert(Huffman.makeOrderedLeafList(List(('a', 3), ('b', 1), ('c', 2))) === List(Leaf('b', 1), Leaf('c', 2), Leaf('a', 3)))
  }

  test("singleton") {
    new TestTrees {
      assert(!Huffman.singleton(List()))
      assert(Huffman.singleton(List(Leaf('a', 1))))
      assert(Huffman.singleton(List(t1)))
      assert(Huffman.singleton(List(t2)))
      assert(!Huffman.singleton(List(t1, t2, Leaf('a', 1))))
      assert(!Huffman.singleton(List(t1, t2)))
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(Huffman.combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(Huffman.decode(t1, Huffman.encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
