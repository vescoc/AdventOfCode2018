package aoc

import scala.annotation.tailrec
import scala.io.Source

object Task01 {
  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("input-01.data")
      .getLines()
      .map { l => l.toInt }
      .toList

    val solution1 = input
      .foldLeft(0){ (acc, value) => acc + value }

    println(s"Solution 1: $solution1")

    @tailrec
    def findTwice(current: Int = 0, set: Set[Int] = Set(), i: List[Int] = input): Option[Int] = {
      if (set.contains(current))
        Some(current)
      else {
        i match {
          case h :: tail =>
            findTwice(current + h, set + current, tail)
          case _ =>
            findTwice(current, set, input)
        }
      }
    }

    val solution2 = findTwice()

    println(s"Solution 2: ${solution2.get}")
  }
}
