package parsing

import scala.io.Source

object Parser {
  val resourcePath = "/home/steve/Documents/libs/FSGSog/advent/src/main/resources/"

  def parseToLines(fileName : String): List[String] = {
    val file = Source.fromFile(resourcePath + fileName)
    val lines = file.getLines().toList
    file.close()
    lines
  }
}
