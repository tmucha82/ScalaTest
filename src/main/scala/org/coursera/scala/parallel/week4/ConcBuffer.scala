package org.coursera.scala.parallel.week4

import scala.reflect.ClassTag

class ConcBuffer[T: ClassTag](k: Int, private var tree: Conc[T]) {
  private var chunk: Array[T] = new Array(k)
  private var chunkSize: Int = 0

  def +=(value: T): ConcBuffer[T] = {
    if (chunkSize == k) {
      expand()
    }
    chunk(chunkSize) = value
    chunkSize += 1
    this
  }

  def expand(): Unit = {
    tree = tree.appendLeaf(Chunk(chunk, chunkSize))
    chunk = new Array(k)
    chunkSize = 0
  }

  def result(): Conc[T] = {
    tree = tree.appendLeaf(Chunk(chunk, chunkSize))
    tree
  }

  def combine(that: ConcBuffer[T]): ConcBuffer[T] = {
    new ConcBuffer[T](k, this.result() <> that.result())
  }
}

object ConcBuffer {
  def apply[T: ClassTag](k: Int = 16, tree: Conc[T] = Empty): ConcBuffer[T] = {
    new ConcBuffer(k, tree)
  }
}
