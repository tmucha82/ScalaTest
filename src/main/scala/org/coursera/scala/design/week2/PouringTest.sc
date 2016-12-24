import org.coursera.scala.design.week2.Pouring

val problem = new Pouring(Vector(4, 7, 9))
problem.moves
problem.pathSets.take(3).toList
problem.solution(6)