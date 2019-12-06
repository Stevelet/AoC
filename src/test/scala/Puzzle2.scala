import org.scalatest.funsuite.AnyFunSuite
import puzzle2.{IntCodeProgram, Main}

class Puzzle2 extends AnyFunSuite {
  test("Smoke test") {
    Main.main(Array())
  }

  test("Test example 1.1") {
    assertResult(3500) {
      new IntCodeProgram(Array(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)).execute()
    }
  }

  test("Test example 1.2") {
    assertResult(2) {
      new IntCodeProgram(Array(1,0,0,0,99)).execute()
    }
  }

  test("Test example 1.3") {
    assertResult(2) {
      new IntCodeProgram(Array(2,3,0,3,99)).execute()
    }
  }

  test("Test example 1.4") {
    assertResult(2) {
      new IntCodeProgram(Array(2,4,4,5,99,0)).execute()
    }
  }

  test("Test example 1.5") {
    assertResult(30) {
      new IntCodeProgram(Array(1,1,1,4,99,5,6,0,99)).execute()
    }
  }
}
