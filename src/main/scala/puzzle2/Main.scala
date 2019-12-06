package puzzle2

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "/home/steve/Documents/libs/FSGSog/advent/src/main/resources/puzzle2.txt"
    val file = Source.fromFile(filename)
    val lines = file.getLines().toList.reduce((l, r) => l + r)
    val program = IntCodeProgramFactory.createIntCodeProgram(lines)

    for (x <- 0 to 100) {
      for (y <- 0 to 100) {
        val new_program = program.clone()
        new_program.writeToMemory(1, x)
        new_program.writeToMemory(2, y)
        val value = new_program.execute()
        if (value == 19690720) println(x + " : " + y)
      }
    }
  }
}
