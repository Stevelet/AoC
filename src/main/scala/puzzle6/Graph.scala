package puzzle6

import scala.io.Source

object Graph {
  def getPathToStart(key : String, map: Map[String, String]) : List[String] =
    if (map.get(key).nonEmpty) getPathToStart(map(key), map) ::: List(key) else List(key)

  def findCommonRoot(left : String, right : String, map: Map[String, String]) : Int =
    getPathToStart(map(left), map)
      .zipAll(getPathToStart(map(right), map), "", "")
      .dropWhile(t => t._1.equals(t._2))
      .map(t => (Math.min(t._1.length, 1), Math.min(t._2.length, 1)))
      .map(t => t._1 + t._2).sum

  def main(args: Array[String]): Unit = {
    val filename = "/home/steve/Documents/libs/FSGSog/advent/src/main/resources/puzzle6.txt"
    val file = Source.fromFile(filename)
    val lines = file.getLines().toList
    file.close()

    val map = lines.map(_.split(')')).map(t => (t(1), t(0))).toMap

    println(map.map((t : (String, String)) => getPathToStart(t._1, map).length - 1).sum)
    println(findCommonRoot("YOU", "SAN", map))
  }
}
