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

  test("makeOrderedLeafList for some frequency table") {
    assert(Huffman.makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
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
