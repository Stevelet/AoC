package puzzle5

import parsing.Parser

object main {
  def main(args: Array[String]): Unit = {
    Parser.parseToLines("puzzle5.txt").reduce((l, r) => l + r)
    file.close()

    val instructions = lines.split(',').map((i : String) => Integer.parseInt(i.toString)).toList

    val assembly = new IntegerAssembly(instructions.toArray)

    println(assembly.getMemoryState)
    while (!assembly.executeNextInstruction()) {
      println(assembly.getMemoryState)
    }
    println(assembly.output)
  }
}

class IntegerAssembly(memory: Array[Int]) {

  case class Operation(argumentCount: Int, outCount: Int, operation: (Seq[Int], Int) => Unit)

  var instructionPointer = 0
  var done = false
  var output = ""

  val operations: Map[Int, Operation] = Map(
    1 -> Operation(2, 1, (arguments, outIndex) => memory(outIndex) = arguments.head + arguments(1)),
    2 -> Operation(2, 1, (arguments, outIndex) => memory(outIndex) = arguments.head * arguments(1)),
    3 -> Operation(0, 1, (_, outIndex) => memory(outIndex) = scala.io.StdIn.readInt()),
    4 -> Operation(1, 0, (arguments, _) => output += arguments.head.toString),
    5 -> Operation(2, 0, (arguments, _) => if (arguments.head != 0) instructionPointer = arguments(1)),
    6 -> Operation(2, 0, (arguments, _) => if (arguments.head == 0) instructionPointer = arguments(1)),
    7 -> Operation(2, 1, (arguments, outIndex) => memory(outIndex) = if (arguments.head < arguments(1)) 1 else 0),
    8 -> Operation(2, 1, (arguments, outIndex) => memory(outIndex) = if (arguments.head == arguments(1)) 1 else 0),
    99 -> Operation(0, 0, (_, _) => done = true)
  )

  def getValues(arguments: List[(Int, Boolean)]): List[Int] =
    arguments.map(t => if (t._2) t._1 else memory(t._1))

  def padCell(cell: String): String =
    "0" * (4 - cell.length) + cell


  def getPassingArray(passingMode: String, requiredLength: Int): List[Boolean] =
    (passingMode + ("0" * Math.max(0, requiredLength - passingMode.length))).map(_ == '1').toList

  def getArguments(argumentCount: Int): List[Int] =
    (0 until argumentCount).map(i => memory(instructionPointer + i)).toList

  def getMemoryState: List[String] =
    memory.toList.zipWithIndex.map(t => if (t._2 == instructionPointer) "<" + getInstruction(instructionPointer) + ">" else t._1.toString)

  def getInstruction(pointer: Int): String =
    padCell(memory(pointer).toString)


  def executeNextInstruction(): Boolean = {
    if (done) return done

    val cell = getInstruction(instructionPointer)
    instructionPointer += 1

    val operationCode = Integer.parseInt(cell.takeRight(2))
    if (operationCode == 99) return true

    val operation = operations(operationCode)
    val arguments = getArguments(operation.argumentCount)
    val passingMode = getPassingArray(cell.dropRight(2).reverse, operation.argumentCount)
    val values = getValues(arguments.zip(passingMode))

    instructionPointer += operation.argumentCount

    if (operation.outCount > 0) {
      val outIndex = memory(instructionPointer)
      instructionPointer += 1
      operation.operation(values, outIndex)
    } else {
      operation.operation(values, 0)
    }

    done
  }
}
