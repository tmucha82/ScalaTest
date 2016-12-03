package org.coursera.week4.assignment.patmat

import org.coursera.week4.assignment.patmat.Huffman._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.List

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  trait TestCodeTables {
    val abc = List(('c', List(0)), ('a', List(1, 0)), ('b', List(1, 1)))
    val etx = List(('e', List(0, 0)), ('t', List(0, 1)), ('x', List(1)))
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
    assert(Huffman.combine(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
    assert(Huffman.combine(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4))) === List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)))
    assert(Huffman.combine(List(Leaf('a', 2), Leaf('b', 3), Leaf('c', 4))) === List(Leaf('c', 4), Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)))
    assert(Huffman.combine(List(Leaf('c', 4), Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5))) === List(Fork(Leaf('c', 4), Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), List('c', 'a', 'b'), 9)))
  }

  test("combine of a singleton or nil") {
    assert(Huffman.combine(List()) === List())
    assert(Huffman.combine(Nil) === Nil)
  }

  test("until") {
    assert(List(Fork(Leaf('c', 4), Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), List('c', 'a', 'b'), 9)) === Huffman.until(Huffman.singleton, Huffman.combine)(List(Leaf('a', 2), Leaf('b', 3), Leaf('c', 4))))
    assert(List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)) === Huffman.until(Huffman.singleton, Huffman.combine)(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))))
  }

  test("createCodeTree") {
    assert(Fork(Leaf('c', 4), Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), List('c', 'a', 'b'), 9) === Huffman.createCodeTree(Huffman.string2Chars("aabbbcccc")))
    assert(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7) === Huffman.createCodeTree(Huffman.string2Chars("ettxxxx")))
  }

  test("decode") {
    assert(Huffman.string2Chars("caba") === Huffman.decode(Huffman.createCodeTree(Huffman.string2Chars("aabbbcccc")), List(0, 1, 0, 1, 1, 1, 0)))
    assert(Huffman.string2Chars("xtet") === Huffman.decode(Huffman.createCodeTree(Huffman.string2Chars("ettxxxx")), List(1, 0, 1, 0, 0, 0, 1)))
  }

  test("decode secret using frenchCode") {
    assert(Huffman.string2Chars("huffmanestcool") === Huffman.decodedSecret)
  }

  test("encode") {
    assert(List(0, 1, 0, 1, 1, 1, 0) === Huffman.encode(Huffman.createCodeTree(Huffman.string2Chars("aabbbcccc")))(Huffman.string2Chars("caba")))
    assert(List(1, 0, 1, 0, 0, 0, 1) === Huffman.encode(Huffman.createCodeTree(Huffman.string2Chars("ettxxxx")))(Huffman.string2Chars("xtet")))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(Huffman.decode(t1, Huffman.encode(t1)("ab".toList)) === "ab".toList)
      assert(Huffman.decode(t2, Huffman.encode(t2)("abddba".toList)) === "abddba".toList)
      assert(Huffman.encode(Huffman.frenchCode)("huffmanestcool".toList) === Huffman.secret)
    }
  }

  test("codeBits") {
    new TestCodeTables {
      assert(List(1, 0) === Huffman.codeBits(abc)('a'))
      assert(List(1, 1) === Huffman.codeBits(abc)('b'))
      assert(List(0) === Huffman.codeBits(abc)('c'))

      assert(List(0, 0) === Huffman.codeBits(etx)('e'))
      assert(List(0, 1) === Huffman.codeBits(etx)('t'))
      assert(List(1) === Huffman.codeBits(etx)('x'))
    }
  }

  test("quickEncode") {
    new TestTrees {
      assert(Huffman.decode(t1, Huffman.quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(Huffman.decode(t2, Huffman.quickEncode(t2)("abddba".toList)) === "abddba".toList)
      assert(Huffman.quickEncode(Huffman.frenchCode)("huffmanestcool".toList) === Huffman.secret)
    }
  }

}
