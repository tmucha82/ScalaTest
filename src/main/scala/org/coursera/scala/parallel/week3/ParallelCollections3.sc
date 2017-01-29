import scala.collection.concurrent.TrieMap
import scala.collection.mutable

{
  //Example 1
  // ▶ Never write to a collection that is concurrently traversed.
  // ▶ Never read from a collection that is concurrently modified.

  // TrieMap is an exception to these rules.
  // The snapshot method can be used to efficiently grab the current state:

  val graph = mutable.Map[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
  graph(graph.size - 1) = 0

  for ((k, v) <- graph.par) graph(k) = graph(v)
  val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })
  println(s"violation: $violation")
}


{
  //Example 2
  val graph = TrieMap[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
  graph(graph.size - 1) = 0

  val previous = graph.snapshot() // we need to take shnaphot

  for ((k, v) <- graph.par) graph(k) = previous(v)
  val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })
  println(s"violation: $violation")
}