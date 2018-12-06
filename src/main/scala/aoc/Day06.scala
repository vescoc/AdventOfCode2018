package aoc

import scala.io.Source

object Day06 {
  val PointRe = """(\d+), (\d+)""".r

  case class Point(x: Int, y: Int) {
    def distanceFrom(point: Point): Int =
      Math.abs(point.x - x) + Math.abs(point.y - y)
  }

  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("input-06.data")
      .getLines()
      .map {
        _ match {
          case PointRe(x, y) => Point(x.toInt, y.toInt)
        }
      }
      .toList

    val (pointMin, pointMax) = input
      .foldLeft((Point(Int.MaxValue, Int.MaxValue), Point(Int.MinValue, Int.MinValue))) { (acc, value) =>
        (
          Point(Math.min(acc._1.x, value.x), Math.min(acc._1.y, value.y)),
          Point(Math.max(acc._2.x, value.x), Math.max(acc._2.y, value.y))
        )
      }

    val (width, height) = (
      Math.abs(pointMax.x - pointMin.x),
      Math.abs(pointMax.y - pointMin.y)
    )

    def area(width: Int, height: Int) =
      (for {
        x <- (pointMin.x - width) to (pointMax.x + width)
        y <- (pointMin.y - height) to (pointMax.y + height)
      } yield Point(x, y))
        .foldLeft(Map.empty[Point, Int]) { (acc, value) =>
          val scan = input
            .foldLeft[(Option[Point], Int)]((None, Int.MaxValue)) { (acc, current) =>
              val distance = current.distanceFrom(value)
              if (distance < acc._2)
                (Some(current), distance)
              else if (distance == acc._2)
                (None, distance)
              else
                acc
            }

          scan match {
            case (None, _) => acc
            case (Some(point), distance) =>
              acc + (point -> (acc.getOrElse(point, 0) + 1))
          }
        }

    val area1 = area(width, height).toMap
    val area2 = area(width + 1, height + 1).toMap

    val solution1 = area1
      .foldLeft(0) { (acc, value) =>
        if (area2(value._1) == value._2)
          if (value._2 > acc)
            value._2
          else
            acc
          else
            acc
      }

    println(s"solution 1: $solution1")

    def region(width: Int, height: Int, limit: Int = 10000) =
      (
        for {
          x <- (pointMin.x - width) to (pointMax.x + width)
          y <- (pointMin.y - height) to (pointMax.y + height)
        } yield {
          val point = Point(x, y)

          input
            .map { point.distanceFrom(_) }
            .sum
        }
      )
        .filter(_ < limit)
        .size

    val solution2 = region(width, height)

    println(s"solution 2: $solution2")
  }
}
