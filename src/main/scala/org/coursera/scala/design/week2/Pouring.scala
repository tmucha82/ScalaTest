package org.coursera.scala.design.week2

/**
  * @param capacity of available glasses
  */
class Pouring(capacity: Vector[Int]) {

  // States - states of all glasses
  type State = Vector[Int]

  val initialState = capacity.map(capacity => 0)

  // Moves - glass, from, to is number of glass
  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    override def change(state: State): State = state.updated(glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state.updated(glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state.updated(from, state(from) - amount).updated(to, state(to) + amount)
    }
  }

  val glasses = capacity.indices

  val moves = {
    (for (glass <- glasses) yield Empty(glass)) ++
      (for (glass <- glasses) yield Fill(glass)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))
  }


  // Paths - sequences of moves
  class Path(history: List[Move]) {
    def endState: State = trackState(history)

    //    def trackState(moves: List[Move]): State = moves match {
    //      case List() => initialState
    //      case move::otherMoves => move change trackState(otherMoves)
    //    }
    def trackState(moves: List[Move]): State = history.foldRight(initialState)(_ change _)

  }

}
