package aoc

import scala.annotation.tailrec

object Day01 {
  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("input-01.data")
      .getLines()
      .map { l =>
        l.toInt
      }
      .toList

    val solution1 = input
      .foldLeft(0) { (acc, value) =>
        acc + value
      }

    println(s"Solution 1: $solution1")

    @tailrec
    def findTwice(
      current: Int = 0,
      set: Set[Int] = Set(),
      i: List[Int] = input,
      index: Int = 0,
      limit: Int = -1
    ): Option[Int] =
      if (set.contains(current)) {
        Some(current)
      } else if (limit < 0 || index < limit) {
        i match {
          case h :: tail =>
            findTwice(current + h, set + current, tail, index + 1, limit)
          case _ =>
            findTwice(current, set, input, index, limit)
        }
      } else
        None

    val solution2 = findTwice()

    println(s"Solution 2: ${solution2.get}")

    println(s"+3 -2: ${findTwice(i = List(3, -1), limit = 100)}")
  }
}
