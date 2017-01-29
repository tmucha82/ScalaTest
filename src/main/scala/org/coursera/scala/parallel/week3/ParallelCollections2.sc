import java.util.concurrent.ConcurrentSkipListSet

import scala.collection.{GenSet, mutable, GenSeq}

// Example 1
{
  def largestPalindrome(xs: GenSeq[Int]): Int = {
    xs.aggregate(Int.MinValue)(
      (largest, n) =>
        if (n > largest && n.toString == n.toString.reverse) n else largest,
      math.max
    )
  }
  //seq
  val array = (0 until 1000).toArray
  largestPalindrome(array)
  //...or par
  largestPalindrome(array.par)
}

// Example 2
{
  def intersection(a: GenSet[Int], b: GenSet[Int]): mutable.Set[Int] = {
    val result = mutable.Set[Int]()
    for (x <- a) if (b contains x) result += x //!!!! - this is worong, becasue param could be par collection and this is clearly mutable Set! - not thread safe
    result
  }
  val seqResult = intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
  val parResult = intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)
  println(s"seqResult = ${seqResult.size}")
  println(s"parResult = ${parResult.size}")
}

//but there is solution!! - ConcurrentSkipListSet
// Example 3
{
  def intersection(a: GenSet[Int], b: GenSet[Int]) = {
    val result = new ConcurrentSkipListSet[Int]()
    for (x <- a) if (b contains x) result.add(x)
    result
  }
  val seqResult = intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
  val parResult = intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)
  println(s"seqResult = ${seqResult.size}")
  println(s"parResult = ${parResult.size}")
}

//another solution!! - ConcurrentSkipListSet
// Example 4
{
  def intersection(a: GenSet[Int], b: GenSet[Int]): GenSet[Int] = {
    if (a.size < b.size) a.filter(b(_))
    else b.filter(a(_))
  }
  val seqResult = intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
  val parResult = intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)
  println(s"seqResult = ${seqResult.size}")
  println(s"parResult = ${parResult.size}")

}