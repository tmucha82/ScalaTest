import org.coursera.scala.functional.week3.Cons
import org.coursera.scala.functional.week3.Nil
import org.coursera.scala.functional.week3.List

def nth[T](list: List[T], n: Int): T = {
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) list.head
  else
    nth(list.tail, n - 1)
}

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
nth(list, 0)
nth(list, 1)
nth(list, 2)
nth(list, 4)
nth(list, -1)