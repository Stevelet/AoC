import org.scalatest.funsuite.AnyFunSuite
import puzzle1.FuelCalculations

class Puzzle1 extends AnyFunSuite {
  test("Smoke test") {
    FuelCalculations.main(Array())
  }

  test("Test example 1.1") {
    assertResult(2) {
      FuelCalculations.get_fuel(12)
    }
  }

  test("Test example 1.2") {
    assertResult(2) {
      FuelCalculations.get_fuel(14)
    }
  }

  test("Test example 1.3") {
    assertResult(654) {
      FuelCalculations.get_fuel(1969)
    }
  }

  test("Test example 1.4") {
    assertResult(33583) {
      FuelCalculations.get_fuel(100756)
    }
  }

  test("Test example 2.1") {
    assertResult(2) {
      FuelCalculations.get_fuel_recursive(12)
    }
  }

  test("Test example 2.2") {
    assertResult(966) {
      FuelCalculations.get_fuel_recursive(1969)
    }
  }

  test("Test example 2.3") {
    assertResult(50346) {
      FuelCalculations.get_fuel_recursive(100756)
    }
  }
}
