package puzzle2

class IntCodeProgram(memory : Array[Int]) extends Iterator[Int] {
  private var currentIndex = 0

  def execute(): Int = {
    var value = next()
    while (hasNext) value = next()
    value
  }

  def writeToMemory(index: Int, value: Int) : Unit =
    memory(index) = value

  override def clone: IntCodeProgram =
    new IntCodeProgram(memory)

  override def hasNext: Boolean =
    memory(currentIndex) != 99

  override def next(): Int = {
    if (!hasNext) return memory(0)

    if (currentIndex >= memory.length) return memory(0)

    val op = memory(currentIndex)

    currentIndex += 1
    val left_index = memory(currentIndex)
    currentIndex += 1
    val right_index = memory(currentIndex)
    currentIndex += 1
    val out_index = memory(currentIndex)
    currentIndex += 1

    if (left_index >= memory.length) return memory(0)
    if (right_index >= memory.length) return memory(0)
    if (out_index >= memory.length) return memory(0)

    op match {
      case 1 =>
        memory(out_index) = memory(left_index) + memory(right_index)
      case 2 =>
        memory(out_index) = memory(left_index) * memory(right_index)
      case _ =>
    }

    memory(0)
  }
}
