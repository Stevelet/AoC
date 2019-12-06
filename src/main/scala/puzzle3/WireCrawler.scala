package puzzle3

import scala.collection.mutable

class WireCrawler(instructions: List[String]) extends Iterator[WireCrawler] {
  var scalar: (Int, Int) = (0, 0)
  var steps: Int = 0
  var remaining: Int = 0
  var index = 0
  var grid: mutable.Map[(Int, Int), Int] = mutable.Map[(Int, Int), Int]()
  var location: (Int, Int) = (0, 0)

  def loadInstruction(): Unit = {
    val instruction = instructions(index)

    remaining = Integer.parseInt(instruction.tail)

    instruction.head match {
      case 'U' => scalar = (0, 1)
      case 'R' => scalar = (1, 0)
      case 'L' => scalar = (-1, 0)
      case 'D' => scalar = (0, -1)
    }
  }

  def executeTillNext(): Unit = {
    while (remaining > 0) {
      location = (location._1 + scalar._1, location._2 + scalar._2)
      steps += 1

      if (grid.get(location).isEmpty) grid.put(location, steps)

      remaining -= 1
    }
    index += 1
  }

  def traceWire(): WireCrawler = {
    while (hasNext) next()
    this
  }

  def printToConsole(): WireCrawler = {
    val minX = grid.toList.map(_._1._1).min
    val maxX = grid.toList.map(_._1._1).max
    val minY = grid.toList.map(_._1._2).min
    val maxY = grid.toList.map(_._1._2).max
    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        if (grid.contains((x, y))) {
          print("*")
        } else {
          print(" ")
        }
      }
      println()
    }
    this
  }

  def getGridMap: Map[(Int, Int), Int] =
    grid.toMap

  override def hasNext: Boolean =
    instructions.length > index

  override def next(): WireCrawler = {
    if (!hasNext) return this
    loadInstruction()
    executeTillNext()
    this
  }
}
