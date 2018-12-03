package aoc

import scala.io.Source
import scala.collection.mutable

object Day03 {
  val re = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  case class Rectangle(
    id: Int,
    x: Int,
    y: Int,
    w: Int,
    h: Int
  )

  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("input-03.data")
      .getLines()
      .map { l =>
        l match {
          case re(id, x, y, w, h) => Rectangle(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
        }
      }
      .toList

    val map = mutable.Map[(Int, Int), mutable.Set[Int]]()
    val solution1 = input
      .foldLeft(0) { (acc, r) =>
        var a = acc

        for (x <- r.x until (r.x + r.w)) {
          for (y <- r.y until (r.y + r.h)) {
            map.get((x, y)) match {
              case None =>
                map((x, y)) = mutable.Set(r.id)
              case Some(set) =>
                if (set.size == 1)
                  a = a + 1
                set += r.id
            }
          }
        }

        a
      }

    println(s"solution 1: $solution1")

    val solution2 = {
      val ids = input
        .foldLeft(Set.empty[Int]) { (acc, r) =>
          acc + r.id
        }

      val overlapIds = map
        .foldLeft(Set.empty[Int]) { (acc, p) =>
          if (p._2.size > 1)
            acc ++ p._2
          else
            acc
        }

      ids -- overlapIds
    }

    println(s"solution 2: $solution2")
  }
}
