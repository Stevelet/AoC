package puzzle3

import java.io.{BufferedWriter, File, FileWriter}

import parsing.Parser

object WireCrawlerManager {
  def main(args: Array[String]): Unit = {
    val lines = Parser.parseToLines("puzzle3.txt")

    val writer = new BufferedWriter(new FileWriter(new File(Parser.resourcePath + "puzzle3.txt")))
    writer.write(drawWiringDiagram(lines))
    writer.flush()
    writer.close()
  }

  private def tupleDistance(tuple: (Int, Int)): Int =
    Math.abs(tuple._1) + Math.abs(tuple._2)

  def getCrossings(wires: List[String]): List[((Int, Int), Int)] = {
    val gridList = wires.map(_.split(',').toList).flatMap(
      new WireCrawler(_).traceWire().getGridMap.toList
    )

    gridList.groupBy(_._1).map((t : ((Int, Int), List[((Int, Int), Int)])) => (t._1, t._2.map(_._2).sum, t._2.length)).toList.filter(_._3 > 1)
      .map(t => (t._1, t._2))
  }

  def getRangeTuple(list: List[((Int, Int), Int)]): (Int, Int, Int, Int) = {
    (list.map(_._1._1).min, list.map(_._1._1).max, list.map(_._1._2).min, list.map(_._1._2).max)
  }

  def drawWiringDiagram(wires: List[String]): String = {
    val drawMap = wires.map(_.split(',').toList)
      .flatMap(new WireCrawler(_).traceWire().getGridMap).toMap

    val rangeTuple = getRangeTuple(drawMap.toList)

    val crossings = getCrossings(wires).toMap

    var string = ""
    for (y <- rangeTuple._3 to rangeTuple._4) {
      for (x <- rangeTuple._1 to rangeTuple._2) {
        if (x == 0 && y == 0) {
          string += "O"
        } else if (crossings.contains((x, y))) {
          string += "#"
        } else if (drawMap.contains((x, y))) {
          string += "+"
        } else {
          string += " "
        }
      }
      string += "\n"
    }
    string
  }


  def calculateNearestCrossing(wires: List[String]): ((Int, Int), Int) =
    getCrossings(wires).reduce((l, r) => if (tupleDistance(l._1) > tupleDistance(r._1)) r else l)

  def calculateStepsToNearest(wires: List[String]): Int =
    calculateNearestCrossing(wires)._2

  def calculateStepsToBest(wires: List[String]): Int =
    getCrossings(wires).map(_._2).min

  def calculateDistanceToNearest(wires: List[String]): Int =
    tupleDistance(calculateNearestCrossing(wires)._1)


}
