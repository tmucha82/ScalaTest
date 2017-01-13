trait Task[T] {
  def join(): T
}

object Task {
  def apply[T](expression: => T) = new Task[T] {
    //of course it is not proper implementation - but just for now
    // it is only for designing issue
    override def join(): T = expression
  }

  implicit def getJoin[T](x: Task[T]): T = {
    println("implicit getJoin")
    x.join()
  }
}

def parallel[A, B](a: => A, b: => B): (A, B) = {
  // we can do that - because of implicit
  // (a, Task(b)) //implicit invocation of getJoin()
  (a, Task(b).join())
}
parallel(2 + 2, 3 + 3)
parallel(2 * 2, 3 - 3)

//example with parallel sum

def power(x: Int, p: Double): Int = Math.exp(p * Math.log(Math.abs(x))).toInt

def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  var i = s
  var sum: Int = 0
  while (i < t) {
    sum = sum + power(a(i), p)
    i = i + 1
  }
  sum
}

def pNormWithParallel(a: Array[Int], p: Double): Int = {
  val m1 = a.length / 4
  val m2 = a.length / 2
  val m3 = 3 * a.length / 4
  val ((sum1, sum2), (sum3, sum4)) = parallel(
    parallel(sumSegment(a, p, 0, m1), sumSegment(a, p, m1, m2)),
    parallel(sumSegment(a, p, m2, m3), sumSegment(a, p, m3, a.length))
  )
  power(sum1 + sum2 + sum3 + sum4, 1 / p)
}

def pNormWithTask(a: Array[Int], p: Double): Int = {
  val m1 = a.length / 4
  val m2 = a.length / 2
  val m3 = 3 * a.length / 4
  val t1 = Task(sumSegment(a, p, 0, m1))
  val t2 = Task(sumSegment(a, p, m1, m2))
  val t3 = Task(sumSegment(a, p, m2, m3))
  val t4 = Task(sumSegment(a, p, m3, a.length))

  //power(t1.join() + t2 + t3 + t4, 1 / p)
  power(t1.join() + t2.join() + t3.join() + t4.join(), 1 / p)
}
pNormWithParallel(Array(1, 2, 3, 4, 5, 6), 2)
pNormWithTask(Array(1, 2, 3, 4, 5, 6), 2)
