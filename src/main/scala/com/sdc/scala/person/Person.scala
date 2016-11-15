package com.sdc.scala.person

class Person(val firstName: String, val lastName: String) extends Ordered[Person] with Equals {

  def compare(that: Person) = {
    val lastNameComparison = lastName.compareToIgnoreCase(that.lastName)
    if (lastNameComparison != 0)
      lastNameComparison
    else
      firstName.compareToIgnoreCase(that.firstName)
  }


  override def toString = firstName + " " + lastName

  override def canEqual(that: Any): Boolean = that match {
    case _: Person => true
    case _ => false
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case person: Person => canEqual(person) && this.firstName == person.firstName && this.lastName == person.lastName
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(firstName, lastName)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Person {
  def orderedMergeSort[T <: Ordered[T]](xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(orderedMergeSort(ys), orderedMergeSort(zs))
    }
  }

}