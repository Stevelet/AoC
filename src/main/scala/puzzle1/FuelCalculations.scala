package puzzle1

import scala.io.Source

object FuelCalculations {
  def main(args: Array[String]): Unit = {
    val filename = "/home/steve/Documents/libs/FSGSog/advent/src/main/resources/puzzle1.txt"
    val file = Source.fromFile(filename)
    val lines = file.getLines().toList
    file.close()

    val values = lines.map(s => Integer.parseInt(s))
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
