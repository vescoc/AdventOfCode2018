package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day02 {
  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("input-02.data")
      .getLines()
      .toList

    val solution1 = input
      .map { l =>
        val set = l
          .groupBy { c =>
            c
          }
          .map { v =>
            (v._1, v._2.size)
          }
          .foldLeft(Set[Int]()) { (acc, value) =>
            acc + value._2
          }

        def exists(n: Int) =
          if (set.contains(n))
            1
          else
            0

        (exists(2), exists(3))
      }
      .foldLeft[Option[(Int, Int)]](Some((0, 0))) { (acc, value) =>
        for {
          a <- acc
        } yield { (a._1 + value._1, a._2 + value._2) } // pigrizia :P
      }
      .map { p =>
        p._1 * p._2
      }

    println(s"solution 1: ${solution1.get}")

    @tailrec
    def solve2(index: Int = 0): String = {
      def helper(index: Int = index, input: List[String] = input) =
        input
          .map { l =>
            new StringBuilder(l).deleteCharAt(index).toString
          }
          .groupBy { l =>
            l
          }
          .map { p =>
            (p._1, p._2.size)
          }
          .filter { p =>
            p._2 > 1
          }

      val map = helper()
      val size = map.size
      size match {
        case 0 => solve2(index + 1)
        case 1 => map.toIterator.next._1
        case _ => sys.error(s"invalid solution: $map")
      }
    }

    println(s"solution 2: ${solve2()}")
  }
}
