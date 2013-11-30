package streams

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class SolverSuite extends FunSuite {

  trait SolverTest extends Solver with StringParserTerrain {
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
  }

  test("done true") {
    new SolverTest {
      assert(done(Block(Pos(4, 7), Pos(4, 7))) === true)
    }
  }

  test("done false") {
    new SolverTest {
      assert(done(Block(Pos(1, 3), Pos(1, 4))) === false)
      assert(done(Block(Pos(0, 3), Pos(0, 3))) === false)
    }
  }

  test("neighborsWithHistory test") {
    new SolverTest {
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).take(2).toSet === Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
    }
  }

  test("newNeighborsOnly test") {
    new SolverTest {
      val newNeighbors = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))))

      assert(newNeighbors === Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream)
    }
  }

  test("from test") {
    new SolverTest {
      val init = neighborsWithHistory(startBlock, List())
      val res = from(init, Set(startBlock))
      assert(res.toList.size === 88)
    }
  }
}
