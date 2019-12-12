package puzzle10

import parsing.Parser

object MonitoringStation {

  case class Asteroid(x: Double, y: Double) {
    def distanceTo(asteroid: Asteroid): Double = {
      val xDistance = Math.abs(x - asteroid.x)
      val yDistance = Math.abs(y - asteroid.y)

      Math.sqrt(Math.pow(xDistance, 2) + Math.pow(yDistance, 2))
    }
  }

  case class Line(from: Asteroid, to: Asteroid) {
    def intersects(asteroid: Asteroid): Boolean = {
      if (asteroid == from || asteroid == to) {
        return false
      }

      val distLeft = from.distanceTo(asteroid)
      val distRight = to.distanceTo(asteroid)
      val distTotal = from.distanceTo(to)

      Math.abs(distLeft + distRight - distTotal) <= 0.000001
    }
  }

  class Laser(origin: Asteroid, radius: Double, startDegree: Double) {
    private var degree = startDegree
    private var hasHit: Boolean = true
    var done: Boolean = false
    var destroyed: List[Asteroid] = List()

    def fire(asteroids: List[Asteroid]): List[Asteroid] = {
      val xMultiplier = Math.sin((degree / 180) * Math.PI)
      val yMultiplier = Math.cos((degree / 180) * Math.PI)

      val targetX = origin.x + xMultiplier * radius
      val targetY = origin.y + yMultiplier * radius

      val laserLine = Line(origin, Asteroid(targetX, targetY))

      val hitList = asteroids.filter(laserLine.intersects)

      if (hitList.nonEmpty) {
        val hit = hitList.minBy(origin.distanceTo)
        hasHit = true

        println("Hit asteroid : " + hit)

        destroyed = hit :: destroyed

        asteroids.filter(_ != hit)
      } else {
        asteroids
      }
    }

    def rotate(): Unit = {
      degree += 0.05
      if (degree >= 360) {
        if (!hasHit) done = true
        hasHit = false
        degree = 0
      }
    }

    def hitCount(): Int =
      destroyed.size
  }

  def main(args: Array[String]): Unit = {
    val asteroids = Parser.parseToLines("puzzle10.txt").map(_.zipWithIndex).zipWithIndex
      .map(t => (t._2, t._1.filter(_._1 == '#').toList))
      .flatMap(t1 => t1._2.map(t2 => Asteroid(t2._2, t1._1)))

    val minX = asteroids.minBy(_.x).x
    val maxX = asteroids.maxBy(_.x).x
    val minY = asteroids.minBy(_.y).y
    val maxY = asteroids.maxBy(_.y).y

    val largest = asteroids.map(a => (a, asteroids.filter(_ != a).map(b => (Line(a, b), asteroids))))
      .map(c => (c._1, c._2.map(lines => lines._2.map(lines._1.intersects).reduce((l, r) => l || r)).count(_ == false)))
      .maxBy(_._2)

    println(largest)

    val furthestX = if (Math.abs(largest._1.x - minX) > Math.abs(largest._1.x - maxX)) minX else maxX
    val furthestY = if (Math.abs(largest._1.y - minY) > Math.abs(largest._1.y - maxY)) minY else maxY

    val radius = largest._1.distanceTo(Asteroid(furthestX, furthestY))

    val laser = new Laser(largest._1, radius, 0.0)
    var targets = asteroids

    while (laser.hitCount() < 200 && targets.nonEmpty && !laser.done) {
      targets = laser.fire(targets)
      laser.rotate()
    }

    println(laser.destroyed)
    println(laser.destroyed.size)
  }
}
