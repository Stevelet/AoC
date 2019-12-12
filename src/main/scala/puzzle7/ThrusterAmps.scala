package puzzle7

import parsing.Parser
import puzzle5.IntegerAssembly

object ThrusterAmps {


  def progressToNext(amplifiers: List[Int], instructions: List[Int]): Int = {
    val assembly = if (amplifiers.tail.isEmpty) {
      new IntegerAssembly(instructions.toArray, List(amplifiers.head, 0))
    } else {
      new IntegerAssembly(instructions.toArray, List(amplifiers.head, progressToNext(amplifiers.tail, instructions)))
    }
    while (!assembly.executeNextInstruction()) {
      println(assembly.getMemoryState)
    }
    assembly.output.head
  }

  @scala.annotation.tailrec
  def runRange(assemblies: Array[IntegerAssembly], previousOutput: Int): Int = {
    assemblies(0).addInput(previousOutput)
    val firstOutput = assemblies(0).runTillOutput()
    assemblies(1).addInput(firstOutput)
    val secondOutput = assemblies(1).runTillOutput()
    assemblies(2).addInput(secondOutput)
    val thirdOutput = assemblies(2).runTillOutput()
    assemblies(3).addInput(thirdOutput)
    val forthOutput = assemblies(3).runTillOutput()
    assemblies(4).addInput(forthOutput)
    val output = assemblies(4).runTillOutput()

    if (output == previousOutput) {
      output
    } else {
      runRange(assemblies, output)
    }
  }

  def setupRange(amplifiers: Array[Int], instructions: List[Int], init : Int): Int = {
    val a = new IntegerAssembly(instructions.toArray, List(amplifiers(0), init))
    val firstOutput = a.runTillOutput()
    val b = new IntegerAssembly(instructions.toArray, List(amplifiers(1), firstOutput))
    val secondOutput = b.runTillOutput()
    val c = new IntegerAssembly(instructions.toArray, List(amplifiers(2), secondOutput))
    val thirdOutput = c.runTillOutput()
    val d = new IntegerAssembly(instructions.toArray, List(amplifiers(3), thirdOutput))
    val fourthOutput = d.runTillOutput()
    val e = new IntegerAssembly(instructions.toArray, List(amplifiers(4), fourthOutput))
    val fifthOutput = e.runTillOutput()
    runRange(List(a, b, c, d, e).toArray, fifthOutput)
  }

  def main(args: Array[String]): Unit = {
    val permutations = (0 until 5).permutations.toList.map(t => t.toList)
    //val instructions = Parser.parseToLines("puzzle7.txt").reduce((l, r) => l + r).split(',').map(Integer.parseInt).toList
    val instructions = List(3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
      27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5)

    println(permutations.map(l => (l, setupRange(l.toArray, instructions, 0))).reduce((l, r) => if (l._2 > r._2) l else r))
  }
}
