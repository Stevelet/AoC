package puzzle8

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import parsing.Parser

object ImageDecoder {
  def main(args: Array[String]): Unit = {
    val width = 25
    val height = 6
    val layerSize = width * height
    val input = Parser.parseToLines("puzzle8.txt").reduce((l, r) => l + r)
    //val input = "0222112222120000"
    val layers = input.grouped(layerSize)
      .map(_.map((c: Char) => Integer.parseInt(c.toString)).toList).toList

    val a = layers.reduce((l, r) => if (l.count(_ == 0) > r.count(_ == 0)) r else l)
      .filter(i => i == 1 || i == 2)
      .map(i => (i, 1)).groupBy(_._1).toList.map(t => (t._1, t._2.length))
      .map(_._2).product
    //println(a)

    val b = layers.flatMap(_.zipWithIndex)
      .groupBy(_._2).toList.sortBy(_._1)
      .map(t => (t._1, t._2.map(_._1)))
      .map(t => (t._1, t._2.reduce((l, r) => if (l == 2) r else l)))
      .map(_._2).grouped(width).toList.map(_.map(i => if (i == 0) " " else 254.toChar.toString).reduce((l, r) => l + r)).reduce((l, r) => l + "\n" + r)

    println(b)
  }
}
