package org.coursera.scala.design.week2.assignment

import org.coursera.scala.design.week2.assignment.streams.{GameDef, Solver, StringParserTerrain}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level0 extends SolutionChecker {
    /* terrain for level 0*/

    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin

    val optsolution = List(Down, Right, Up)
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("isStanding") {
    new Level1 {
      assert(!Block(Pos(1, 4), Pos(2, 4)).isStanding)
      assert(!Block(Pos(3, 1), Pos(3, 2)).isStanding)
      assert(Block(Pos(4, 6), Pos(4, 6)).isStanding)
      assert(Block(Pos(6, 3), Pos(6, 3)).isStanding)
    }
  }

  test("terrain function level 0") {
    new Level0 {
      assert(terrain(Pos(1, 2)), "1,2") // start
      assert(terrain(Pos(1, 3)), "1,3") // goal
      assert(!terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(2, 2)), "2,2")
      assert(terrain(Pos(3, 3)), "3,3")
      assert(!terrain(Pos(4, 7)), "4,7")
    }
  }

  test("findChar level 0") {
    new Level0 {
      assert(startPos === Pos(1, 2))
      assert(goal === Pos(1, 3))
    }
  }

  test("isLegal level 0") {
    new Level0 {
      assert(Block(Pos(1, 2), Pos(1, 3)).isLegal)
      assert(Block(Pos(1, 2), Pos(1, 2)).isLegal)
      assert(Block(Pos(2, 2), Pos(3, 2)).isLegal)
      assert(!Block(Pos(1, 3), Pos(1, 4)).isLegal)
      assert(!Block(Pos(2, 1), Pos(2, 2)).isLegal)
      assert(!Block(Pos(3, 1), Pos(3, 1)).isLegal)
    }
  }

  test("startBlock level 0") {
    new Level0 {
      assert(startBlock.isStanding)
      assert(startBlock.isLegal)
      assert(Pos(1, 2) === startBlock.b1)
      assert(Pos(1, 2) === startBlock.b2)
    }
  }

  test("legalNeighbors level 0") {
    new Level0 {
      assert(List((Block(Pos(2, 3), Pos(3, 3)), Down)) === Block(Pos(1, 3), Pos(1, 3)).legalNeighbors)
      assert(List((Block(Pos(2, 2), Pos(2, 3)), Down)) === Block(Pos(1, 2), Pos(1, 3)).legalNeighbors)
      assert(List((Block(Pos(1, 2), Pos(1, 2)), Up), (Block(Pos(2, 3), Pos(3, 3)), Right)) === Block(Pos(2, 2), Pos(3, 2)).legalNeighbors)
    }
  }

  test("done level 0") {
    new Level0 {
      assert(!done(Block(Pos(2, 1), Pos(3, 1))))
      assert(!done(Block(Pos(3, 3), Pos(3, 3))))
      assert(!done(Block(Pos(1, 2), Pos(1, 3))))
      assert(done(Block(Pos(1, 3), Pos(1, 3))))
    }
  }

  test("neighborsWithHistory level 0") {
    new Level0 {
      assert(Set(
        (Block(Pos(1, 2), Pos(1, 2)), List(Up, Left, Up)),
        (Block(Pos(2, 3), Pos(3, 3)), List(Right, Left, Up))
      ) === neighborsWithHistory(Block(Pos(2, 2), Pos(3, 2)), List(Left, Up)).toSet)
    }
  }

  test("newNeighborsOnly level 0") {
    new Level0 {
      assert(Set((Block(Pos(2, 3), Pos(3, 3)), List(Right, Left, Up))).toStream ===
        newNeighborsOnly(
          Set(
            (Block(Pos(1, 2), Pos(1, 2)), List(Up, Left, Up)),
            (Block(Pos(2, 3), Pos(3, 3)), List(Right, Left, Up))
          ).toStream,
          Set(Block(Pos(1, 2), Pos(1, 2)), Block(Pos(3, 2), Pos(3, 3)))
        ))
    }
  }

  test("from level 0") {
    new Level0 {
      assert(Stream.Empty === from(Stream.Empty, Set.empty))
      assert(List((Block(Pos(2, 2), Pos(3, 2)), List(Down))) === from(List((Block(Pos(1, 2), Pos(1, 2)), Nil)).toStream, Set.empty).take(1).toList)
    }
  }

  test("pathsFromStart level 0") {
    new Level0 {
      assert(List((Block(Pos(2, 2), Pos(3, 2)), List(Down))) === pathsFromStart.take(1).toList)
    }
  }

  test("pathsToGoal level 0") {
    new Level0 {
      assert(List((Block(Pos(1, 3), Pos(1, 3)), List(Up, Right, Down))) === pathsToGoal.toList)
    }
  }

  test("optimal solution for level 0") {
    new Level0 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 0") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos === Pos(1, 1))
      assert(goal === Pos(4, 7))
    }
  }

  test("isLegal level 1") {
    new Level1 {
      assert(Block(Pos(2, 1), Pos(3, 1)).isLegal)
      assert(!Block(Pos(3, 2), Pos(4, 2)).isLegal)
      assert(Block(Pos(3, 4), Pos(3, 4)).isLegal)
      assert(!Block(Pos(1, 7), Pos(2, 7)).isLegal)
      assert(Block(Pos(4, 6), Pos(4, 7)).isLegal)
      assert(!Block(Pos(5, 8), Pos(5, 9)).isLegal)
      assert(!Block(Pos(3, 10), Pos(3, 10)).isLegal)
    }
  }

  test("startBlock level 1") {
    new Level1 {
      assert(startBlock.isStanding)
      assert(startBlock.isLegal)
      assert(Pos(1, 1) === startBlock.b1)
      assert(Pos(1, 1) === startBlock.b2)
    }
  }

  test("legalNeighbors level 1") {
    new Level1 {
      assert(List((Block(Pos(1, 1), Pos(1, 1)), Up), (Block(Pos(2, 2), Pos(3, 2)), Right)) === Block(Pos(2, 1), Pos(3, 1)).legalNeighbors)
      assert(List((Block(Pos(3, 2), Pos(3, 3)), Left), (Block(Pos(1, 4), Pos(2, 4)), Up), (Block(Pos(3, 5), Pos(3, 6)), Right)) === Block(Pos(3, 4), Pos(3, 4)).legalNeighbors)
      assert(List((Block(Pos(4, 5), Pos(4, 5)), Left), (Block(Pos(3, 6), Pos(3, 7)), Up), (Block(Pos(4, 8), Pos(4, 8)), Right), (Block(Pos(5, 6), Pos(5, 7)), Down)) === Block(Pos(4, 6), Pos(4, 7)).legalNeighbors)
    }
  }

  test("done level 1") {
    new Level1 {
      assert(!done(Block(Pos(2, 1), Pos(3, 1))))
      assert(!done(Block(Pos(3, 3), Pos(3, 3))))
      assert(!done(Block(Pos(4, 7), Pos(4, 8))))
      assert(done(Block(Pos(4, 7), Pos(4, 7))))
    }
  }

  test("neighborsWithHistory level 1") {
    new Level1 {
      assert(Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ) === neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet)
    }
  }

  test("newNeighborsOnly level 1") {
    new Level1 {
      assert(Set((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream ===
        newNeighborsOnly(
          Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)), (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,
          Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))
        ))
    }
  }

  test("from level 1") {
    new Level1 {
      assert(Stream.Empty === from(Stream.Empty, Set.empty))
      assert(List((Block(Pos(1, 2), Pos(1, 3)), List(Right))) === from(List((Block(Pos(1, 1), Pos(1, 1)), Nil)).toStream, Set.empty).take(1).toList)
    }
  }

  test("pathsFromStart level 1") {
    new Level1 {
      assert(List((Block(Pos(1, 2), Pos(1, 3)), List(Right))) === pathsFromStart.take(1).toList)
    }
  }

  test("pathsToGoal level 1") {
    new Level1 {
      assert(
        List(
          (Block(Pos(4, 7), Pos(4, 7)), List(Down, Right, Right, Right, Down, Right, Right)),
          (Block(Pos(4, 7), Pos(4, 7)), List(Right, Down, Down, Right, Right, Down, Right)),
          (Block(Pos(4, 7), Pos(4, 7)), List(Right, Down, Right, Right, Down, Down, Right)))
          === pathsToGoal.toList)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
