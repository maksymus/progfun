package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val e = Leaf('e', 1)
    val t = Leaf('t', 2)
    val x = Leaf('x', 4)
    
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    val tree = Fork(
      Leaf('A', 24), Fork(
        Fork(Leaf('B', 12), Leaf('C', 10), "BC".toList, 22),
        Fork(Leaf('D', 8), Leaf('E', 8), "DE".toList, 16),
        "BCDE".toList, 38),
      "ABCDE".toList, 62)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  
  test("makeCodeTree") {
    new TestTrees {
      val sampleTree = makeCodeTree(
        makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
        Leaf('t', 2))
        
      assert(sampleTree === 
        Fork(
          Fork(Leaf('x', 1), Leaf('e', 1), List('x', 'e'), 2), 
          Leaf('t', 2), List('x', 'e', 't'), 4))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times simple") {
    assert(times(List('a', 'a', 'b')) === List(('a', 2), ('b', 1)))
  }

  test("times complex") {
    assert(times("hello, world".toList) ===  List(('e',1), (' ',1), (',',1), ('l',3), ('h',1), ('r',1), ('w',1), ('o',2), ('d',1)) )
  }

  test("times empty") {
    assert(times(List()) === List())
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton true") {
    assert(singleton(List(Leaf('a', 1))))
  }

  test("singleton false") {
    assert(!singleton(List(Leaf('a', 1), Leaf('b', 2))))
  }
  
  test("combine of some leaf list") {
    new TestTrees {
      assert(combine(List(e, t, x)) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
    }
  }

  test("combine empty list") {
    new TestTrees {
        assert(combine(List()) === List())
    }
  }

  test("combine one element") {
    new TestTrees {
        assert(combine(List(e)) === List(Leaf('e', 1)))
    }
  }

  test("combine two elements") {
    new TestTrees {
      assert(combine(List(e, t)) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3)))
    }
  }

  test("combine ordered") {
    new TestTrees {
      assert(combine(List(t, x, e)) === List(Leaf('e', 1), Fork(Leaf('t', 2), Leaf('x', 4), List('t', 'x'), 6)))
    }
  }
  
  test("until compiles") {
    new TestTrees {
      assert(until(singleton, combine)(List(e, t, x)) === Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7))
    }
  }

 
  test("createCodeTree test") {
    new TestTrees {
      assert(createCodeTree("texxxxt".toList) === Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7))
    }
  }

  test("decode test") {
    new TestTrees {
      val bits = List(0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1)      
      assert("ABCDE" === decode(tree, bits).foldLeft("")((acc, char) => acc + char))
    }
  }

  test("decode secret") {
    new TestTrees {
      assert("huffmanestcool" === decodedSecret.foldLeft("")((acc, char) => acc + char))
    }
  }

  test("encode test") {
    new TestTrees {
      val bits = List(0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1)
      assert(bits === encode(tree)("ABCDE".toList))
    }
  }

  test("quickEncode test") {
    new TestTrees {
      val bits = List(0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1)
      assert(bits === quickEncode(tree)("ABCDE".toList))
    }
  }

  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits test") {
    new TestTrees {
      val table = List(('a', List(1, 0, 1)))
      assert(codeBits(table)('a') === List(1, 0, 1))
    }
  }

  test("codeBits complex test") {
    new TestTrees {
      val table = List(('A', List(0)), ('B', List(1, 0, 0)), ('C', List(1, 0, 1)), ('D', List(1, 1, 0)), ('E', List(1, 1, 1)))
      
      def testCodeBits(char: Char) = codeBits(table)(char) 
      
      assert(testCodeBits('A') === List(0))
      assert(testCodeBits('B') === List(1, 0, 0))
      assert(testCodeBits('C') === List(1, 0, 1))
      assert(testCodeBits('D') === List(1, 1, 0))
      assert(testCodeBits('E') === List(1, 1, 1))
      assert(testCodeBits('X') === List())
    }
  }

  test("convert test") {
    new TestTrees {
      assert(convert(tree) === List(('A', List(0)), ('B', List(1, 0, 0)), ('C', List(1, 0, 1)), ('D', List(1, 1, 0)), ('E', List(1, 1, 1))))
    }
  }

}
