package puzzle2

import parsing.Parser

object Main {
  def main(args: Array[String]): Unit = {
    val lines = Parser.parseToLines("puzzle2.txt").reduce((l, r) => l + r)
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
