package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val emptySet: Set = (x: Int) => false
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersect strait") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("intersect same") {
    new TestSets {
      val same = intersect(s1, s1)
      assert(contains(same, 1), "Intersect same")
    }
  }

  test("intersect complex") {
    new TestSets {
      val complex = intersect(union(s1, s2), s2)
      assert(!contains(complex, 1), "should not contain 1")
      assert(contains(complex, 2), "should contain 2")
      assert(!contains(complex, 3), "should contain 3")
    }
  }

  test("diff strait") {
    new TestSets {
      val set = diff(s1, s2)
      assert(contains(set, 1), "should contain 1")
      assert(!contains(set, 2), "should not contain 2")
      assert(!contains(set, 3), "should not contain 3")
    }
  }

  test("diff complex") {
    new TestSets {
      val set = diff(union(s1, s2), s2)
      assert(contains(set, 1), "should contain 1")
      assert(!contains(set, 2), "should not contain 2")
      assert(!contains(set, 3), "should not contain 3")
    }
  }

  test("filter strait") {
    new TestSets {
      val set = filter(s1, (x: Int) => x > 0)
      assert(contains(set, 1), "should contain 1")
      assert(!contains(set, 2), "should not contain 2")
      assert(!contains(set, 3), "should not contain 3")
    }
  }

  test("filter complex") {
    new TestSets {
      val set = filter(union(union(s1, s2), s3), (x: Int) => x > 1 && x < 3)
      assert(!contains(set, 1), "should not contain 1")
      assert(contains(set, 2), "should contain 2")
      assert(!contains(set, 3), "should not contain 3")
    }
  }

  test("forall odd") {
    new TestSets {
      val set = union(s1, s3)
      val res = forall(set, (x: Int) => x % 2 == 1)
      assert(res, "contains odd only")
    }
  }

  test("forall not all odds") {
    new TestSets {
      val set = union(union(s1, s3), s2)
      val res = forall(set, (x: Int) => x % 2 == 1)
      assert(!res, "not all odds")
    }
  }

  test("exists matched") {
    new TestSets {
      val set = union(union(s1, s3), s2)
      val result = exists(set, (x: Int) => x % 2 == 0)
      assert(result, "s2 should match")
    }
  }

  test("exists not matched") {
    new TestSets {
      val set = union(union(s1, s3), s2)
      val result = exists(set, (x: Int) => x > 3)
      assert(!result, "nothing matches in set")
    }
  }

  test("map double") {
    new TestSets {
      val set = union(s2, s3)
      val mapped = map(set, (x: Int) => x * 2)
      
      assert(!contains(mapped, 2), "should not contain 2")
      assert(!contains(mapped, 3), "should not contain 3")
      assert(contains(mapped, 4), "should contain 4")
      assert(contains(mapped, 6), "should contain 6")
    }
  }

  test("map sum") {
    new TestSets {
      val set = union(s2, s3)
      val mapped = map(set, (x: Int) => x + 10)

      assert(!contains(mapped, 2), "should not contain 2")
      assert(!contains(mapped, 3), "should not contain 3")
      assert(contains(mapped, 12), "should contain 12")
      assert(contains(mapped, 13), "should contain 13")
    }
  }

}
