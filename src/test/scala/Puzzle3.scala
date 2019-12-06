import org.scalatest.funsuite.AnyFunSuite
import puzzle3.WireCrawlerManager.{calculateDistanceToNearest, calculateStepsToBest}

class Puzzle3 extends AnyFunSuite {
  test("Example 1.1 test") {
    assertResult(159) {
      calculateDistanceToNearest(List("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"))
    }
  }

  test("Example 1.2 test") {
    assertResult(135) {
      calculateDistanceToNearest(List("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
    }
  }

  test("Example 1.3 test") {
    assertResult(6) {
      calculateDistanceToNearest(List("R8,U5,L5,D3", "U7,R6,D4,L4"))
    }
  }

  test("Example 2.1 test") {
    assertResult(610) {
      calculateStepsToBest(List("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"))
    }
  }

  test("Example 2.2 test") {
    assertResult(410) {
      calculateStepsToBest(List("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
    }
  }

  test("Example 2.3 test") {
    assertResult(30) {
      calculateStepsToBest(List("R8,U5,L5,D3", "U7,R6,D4,L4"))
    }
  }
}
