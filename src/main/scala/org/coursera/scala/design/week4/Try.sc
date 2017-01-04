import java.io.{PrintWriter, StringWriter, IOException}
import scala.util.{Success, Failure, Try}

class AlsException(s: String, e: Exception) extends Exception(s: String, e: Exception)

def badAdder(a: Int): Try[Int] = {
  Try({
    val b = a + 1
    if (b == 3) b
    else {
      val ioe = new IOException("Boom!")
      throw new AlsException("Bummer!", ioe)
    }
  })
}

def proceedTry(result: Try[Int]) = result match {
  case Success(i) => println(s"success, i = $i")
  case Failure(t) =>
    println(s"CLASS = ${t.getClass}")
    // this works, but it's not too useful/readable
    //println(t.getStackTrace.mkString("\n"))
    // this works much better
    val sw = new StringWriter
    t.printStackTrace(new PrintWriter(sw))
    println(sw.toString)
}

proceedTry(badAdder(2))
proceedTry(badAdder(3))
