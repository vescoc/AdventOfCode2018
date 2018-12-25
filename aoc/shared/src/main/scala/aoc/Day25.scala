package aoc

import scala.annotation.tailrec

object Day25 {
  case class Point(x: Int, y: Int, z: Int, t: Int) {
    @inline def distance(that: Point) =
      Math.abs(x - that.x) + Math.abs(y - that.y) + Math.abs(z - that.z) + Math.abs(t - that.t)
  }

  object Point {
    val pointRe = """(-?\d+)+,(-?\d+)+,(-?\d+)+,(-?\d+)+""".r

    def parse(lines: Iterator[String]) =
      lines.collect {
        case pointRe(x, y, z, t) => Point(x.toInt, y.toInt, z.toInt, t.toInt)
        case line @ _            => sys.error(s"invalid input $line")
      }

    def constellations(points: Iterator[Point], length: Int = 3): List[List[Point]] = {
      @tailrec
      def constellations(nextConstellationId: Int, point2constellations: Map[Point, Int]): Map[Point, Int] =
        if (points.hasNext) {
          val point = points.next

          val near = point2constellations
            .filter { p =>
              (p._1 distance point) <= length
            }
            .map { _._2 }
            .toSet

          near.size match {
            case 0 =>
              constellations(nextConstellationId + 1, point2constellations + (point -> nextConstellationId))
            case 1 =>
              constellations(nextConstellationId, point2constellations + (point -> near.head))
            case _ =>
              constellations(
                nextConstellationId + 1,
                (
                  point2constellations.map { p =>
                    p._1 -> (
                      if (near.contains(p._2))
                        nextConstellationId
                      else
                        p._2
                    )
                  }
                ) + (point -> nextConstellationId)
              )
          }

        } else {
          point2constellations
        }

      constellations(0, Map.empty)
        .foldLeft(Map.empty[Int, List[Point]]) { (acc, p) =>
          acc + (p._2 -> (p._1 :: acc.getOrElse(p._2, List.empty)))
        }
        .map { p =>
          p._2
        }
        .toList
    }
  }

  def main(args: Array[String]): Unit = {
    val constellations = Point.constellations(Point.parse(Source.fromResource("input-25.data").getLines()))

    println(s"solution 1: ${constellations.size}")
  }
}
