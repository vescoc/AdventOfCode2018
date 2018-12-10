package aoc

import scala.annotation.tailrec

object Day06 {
  val PointRe = """(\d+), (\d+)""".r

  case class Point(x: Int, y: Int) {
    def distanceFrom(point: Point): Int =
      Math.abs(point.x - x) + Math.abs(point.y - y)

    def -(point: Point): Point =
      Point(x - point.x, y - point.y)

    def +(point: Point): Point =
      Point(x + point.x, y + point.y)

    def -(value: Int): Point =
      Point(x - value, y - value)

    def +(value: Int): Point =
      Point(x + value, y + value)

    def /(value: Int): Point =
      Point(x / value, y / value)

    def <(point: Point): Boolean =
      x < point.x && y < point.y

    def >(point: Point): Boolean =
      x > point.x && y > point.y
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

    val center = (pointMin + (pointMax - pointMin) / 2)

    val L = Math.max(pointMin.distanceFrom(center), pointMax.distanceFrom(center))

    def makeBorder(l: Int) =
      (
        (
          for {
            x <- (center.x - l) to (center.x + l)
          } yield Point(x, center.y - l)
        ) ++ (
          for {
            x <- (center.x - l) to (center.x + l)
          } yield Point(x, center.y + l)
        ) ++ (
          for {
            y <- (center.y - l) to (center.y + l)
          } yield Point(center.x - l, y)
        ) ++ (
          for {
            y <- (center.y - l) to (center.y + l)
          } yield Point(center.x + l, y)
        )
      ).toSet.toList

    @tailrec
    def area(
      l: Int = 0,
      areaMap: Map[Point, Int] = Map.empty,
      candidateMap: Map[Point, Int] = Map.empty
    ): (Point, Int) = {
      val b = makeBorder(l)

      val border = b
        .map { current =>
          val value = input
            .foldLeft[(Option[Point], Int)](None -> Int.MaxValue) { (acc, point) =>
              val distanceFrom = point.distanceFrom(current)
              if (acc._2 > distanceFrom)
                Some(point) -> distanceFrom
              else if (acc._2 == distanceFrom)
                None -> distanceFrom
              else
                acc
            }

          value
        }

      val newCandidateMap = areaMap -- border.collect { case (Some(point), distance) => point }
      lazy val newAreaMap = border
        .foldLeft(areaMap) { (acc, value) =>
          value match {
            case (Some(point), _) => acc + (point -> (acc.getOrElse(point, 0) + 1))
            case (None, _)        => acc
          }
        }

      if (l > L && newCandidateMap.isEmpty == false && newCandidateMap == candidateMap)
        candidateMap.maxBy { _._2 } else {
        area(l + 1, newAreaMap, newCandidateMap)
      }
    }

    val solution1 = area()._2

    println(s"solution 1: $solution1")

    @tailrec
    def region(l: Int = 0, count: Int = 0, limit: Int = 10000): Int = {
      val b = makeBorder(l)

      val newCount = b
        .map { point =>
          input.map { point.distanceFrom(_) }.sum
        }
        .filter(_ < limit)
        .size

      if (l > L && newCount == 0) {
        count
      } else
        region(l + 1, count + newCount)
    }

    val solution2 = region()

    println(s"solution 2: $solution2")
  }
}
