package puzzle12

import parsing.Parser

import scala.collection.mutable

object NBodyProblem {

  case class Vector(x: Int, y: Int, z: Int) {
    def add(vector: Vector): Vector = Vector(x + vector.x, y + vector.y, z + vector.z)

    def getEnergy: Int = Math.abs(x) + Math.abs(y) + Math.abs(z)
  }

  case class Moon(position: Vector, velocity: Vector) {
    def getEnergy: Int = position.getEnergy * velocity.getEnergy
  }

  class MoonSystem(startMoons: List[Moon]) {
    type HistoryMap = mutable.Map[(List[Int], List[Int]), List[Long]]
    private val xHistory: HistoryMap = mutable.Map()
    private val yHistory: HistoryMap = mutable.Map()
    private val zHistory: HistoryMap = mutable.Map()
    private var moons = startMoons
    private var currentStep: Long = 0
    var found: (Boolean, Boolean, Boolean) = (false, false, false)

    def step(): List[Moon] = {
      moons = updatePositions(updateVelocities(moons))
      currentStep = currentStep + 1
      val x = moons.map(t => (List(t.position.x), List(t.velocity.x))).reduce((l, r) => (l._1 ::: r._1, l._2 ::: r._2))
      val y = moons.map(t => (List(t.position.y), List(t.velocity.y))).reduce((l, r) => (l._1 ::: r._1, l._2 ::: r._2))
      val z = moons.map(t => (List(t.position.z), List(t.velocity.z))).reduce((l, r) => (l._1 ::: r._1, l._2 ::: r._2))

      val newX = currentStep :: xHistory.getOrElse(x, List())
      val newY = currentStep :: yHistory.getOrElse(y, List())
      val newZ = currentStep :: zHistory.getOrElse(z, List())

      if (newX.size > 1) found = (true, found._2, found._3)
      if (newY.size > 1) found = (found._1, true, found._3)
      if (newZ.size > 1) found = (found._1, found._2, true)

      xHistory.put(x, newX)
      yHistory.put(y, newY)
      zHistory.put(z, newZ)

      moons
    }

    def getSmallestRecurring: (List[Long], List[Long], List[Long]) =
      (
        xHistory.toList.filter(_._2.length > 1).map(_._2).head,
        yHistory.toList.filter(_._2.length > 1).map(_._2).head,
        zHistory.toList.filter(_._2.length > 1).map(_._2).head
      )


    def getSystemString: String =
      moons.map(moon => (Vector.unapply(moon.position).get, Vector.unapply(moon.velocity).get))
        .map(t => "pos=<x=" + t._1._1 + ", y=  " + t._1._2 + ", z= " + t._1._3 + ">, vel=<x= " + t._2._1 + ", y= " + t._2._2 + ", z= " + t._2._3 + ">")
        .reduce((l, r) => l + "\n" + r)

    def isDone: Boolean =
      found._1 && found._2 && found._3

    def getSystemEnergy: Int =
      moons.map(_.getEnergy).sum
  }

  def updateVelocities(moons: List[Moon]): List[Moon] = {
    var head = moons.head
    val tail = moons.tail.map(moon => {
      val xVelocity = moon.position.x.compare(head.position.x)
      val yVelocity = moon.position.y.compare(head.position.y)
      val zVelocity = moon.position.z.compareTo(head.position.z)

      head = Moon(head.position, Vector(head.velocity.x + xVelocity, head.velocity.y + yVelocity, head.velocity.z + zVelocity))
      Moon(moon.position, Vector(moon.velocity.x - xVelocity, moon.velocity.y - yVelocity, moon.velocity.z - zVelocity))
    })
    if (tail.nonEmpty) head :: updateVelocities(tail) else List(head)
  }

  def updatePositions(moons: List[Moon]): List[Moon] =
    moons.map(moon => Moon(moon.position.add(moon.velocity), moon.velocity))

  def main(args: Array[String]): Unit = {
    val moonSystem = new MoonSystem(Parser.parseToLines("puzzle12.txt").map(_.split(',').map(_.replaceAll("[^-0-9]", "")))
      .map(l => Vector(Integer.parseInt(l(0)), Integer.parseInt(l(1)), Integer.parseInt(l(2)))).map(v => Moon(v, Vector(0, 0, 0))))

    var index = 0
    while (!moonSystem.isDone) {
      index += 1
      moonSystem.step()
    }
    val dimTuple = moonSystem.getSmallestRecurring

    val periodTuple = (dimTuple._1.head - dimTuple._1.tail.head, dimTuple._2.head - dimTuple._2.tail.head, dimTuple._3.head - dimTuple._3.tail.head)
    println(periodTuple)
  }
}