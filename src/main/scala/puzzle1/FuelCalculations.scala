package puzzle1

import parsing.Parser

object FuelCalculations {
  def main(args: Array[String]): Unit = {
    val values = Parser.parseToLines("puzzle1.txt").map(s => Integer.parseInt(s))
    val required_fuel1 = values.map(_ / 3 - 2).sum
    println(required_fuel1)

    val required_fuel2 = values.map(get_fuel_recursive).sum
    println(required_fuel2)
  }

  def get_fuel(mass : Int) : Int =
    mass / 3 - 2

  def get_fuel_recursive(mass : Int) : Int =
    if (get_fuel(mass) > 0) get_fuel(mass) + get_fuel_recursive(get_fuel(mass)) else 0
}
