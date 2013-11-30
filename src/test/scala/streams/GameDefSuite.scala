package streams

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class GameDefSuite extends FunSuite {

  trait GameDefTest extends GameDef with StringParserTerrain {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin

    val start = Block(Pos(1, 2), Pos(1, 2))
    val left = Block(Pos(1, 0), Pos(1, 1))
    val right = Block(Pos(1, 3), Pos(1, 4))
    val up = Block(Pos(-1, 2), Pos(0, 2))
    val down = Block(Pos(2, 2), Pos(3, 2))
  }

  test("Block.neighbors start block") {
    new GameDefTest {
      assert(start.neighbors === List((left, Left), (right, Right), (up, Up), (down, Down)))
    }
  }

  test("Block.legalNeighbors start block") {
    new GameDefTest {
      assert(start.legalNeighbors === List((down, Down)))
    }
  }
  
  test("Block.isStanding true") {
    new GameDefTest {
      assert(Block(Pos(1, 2), Pos(1, 2)).isStanding === true)
    }
  }

  test("Block.isStanding false") {
    new GameDefTest {
      assert(Block(Pos(1, 2), Pos(1, 3)).isStanding === false)
      assert(Block(Pos(1, 2), Pos(2, 2)).isStanding === false)
    }
  }

  test("Block.isLegal true") {
    new GameDefTest {
      assert(Block(Pos(1, 2), Pos(1, 2)).isLegal === true)
    }
  }

  test("Block.isLegal false") {
    new GameDefTest {
      assert(Block(Pos(0, 0), Pos(0, 0)).isLegal === false)
      assert(Block(Pos(0, 0), Pos(0, 1)).isLegal === false)
      assert(Block(Pos(0, 0), Pos(1, 0)).isLegal === false)
      assert(Block(Pos(2, 0), Pos(2, 1)).isLegal === false)
    }
  }

  test("Block.startBlock") {
    new GameDefTest {
      assert(startBlock === Block(Pos(1, 2), Pos(1, 2)))
    }
  }
}
