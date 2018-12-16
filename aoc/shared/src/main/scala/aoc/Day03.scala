package aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Day03 {
  val Pure = true

  val re = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  case class Rectangle(
    id: Int,
    x: Int,
    y: Int,
    w: Int,
    h: Int
  )

  val input = Source
    .fromResource("input-03.data")
    .getLines()
    .map { l =>
      l match {
        case re(id, x, y, w, h) => Rectangle(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
      }
    }
    .toList

  type Info = Map[(Int, Int), Set[Int]]

  def main(args: Array[String]): Unit =
    if (Pure) {
      val (solution1, map) = input
        .foldLeft((0, Map.empty[(Int, Int), Set[Int]])) { (acc, r) =>
          val (a, map) = acc

          @tailrec
          def helper(a: Int = a, map: Info = map, x: Int = r.x, y: Int = r.y): (Int, Info) = {
            val (na, nmap) = {
              map.get((x, y)) match {
                case None =>
                  (a, map + ((x, y) -> Set(r.id)))
                case Some(set) =>
                  (if (set.size == 1) a + 1 else a, map + ((x, y) -> (set + r.id)))
              }
            }

            val (nx, ny) = {
              if (x + 1 == r.x + r.w)
                (r.x, y + 1)
              else
                (x + 1, y)
            }

            if (ny == r.y + r.h)
              (na, nmap)
            else
              helper(na, nmap, nx, ny)
          }

          helper()
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
    } else {
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
