package streams

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class StringParserTerrainSuite extends FunSuite {

  trait ParserTest extends StringParserTerrain {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  test("terrainFunction '-' not valid") {
    new ParserTest {
      assert(terrain(Pos(0, 0)) === false)
    }
  }

  test("terrainFunction out of bound not valid 1") {
    new ParserTest {
      assert(terrain(Pos(-1, 0)) === false)
    }
  }

  test("terrainFunction out of bound not valid 2") {
    new ParserTest {
      assert(terrain(Pos(0, 0)) === false)
    }
  }
  
  test("terrainFunction 'S' valid") {
    new ParserTest {
      assert(terrain(Pos(1, 2)) === true)
    }
  }

  test("terrainFunction 'T' valid") {
    new ParserTest {
      assert(terrain(Pos(1, 3)) === true)
    }
  }

  test("terrainFunction 'o' valid") {
    new ParserTest {
      assert(terrain(Pos(2, 2)) === true)
      assert(terrain(Pos(2, 3)) === true)
    }
  }

  test("findChar 'S'") {
    new ParserTest {
      assert(startPos === Pos(1, 2))
      assert(goal === Pos(1, 3))
    }
  }
}
