package aoc

object Day23 {
  case class Point(x: Int, y: Int, z: Int)
  object Point {
    def distance(a: Point, b: Point) =
      Math.abs(a.x - b.x) + Math.abs(a.y - b.y) + Math.abs(a.z - b.z)
  }

  case class Nanobot(pos: Point, r: Int) {
    def distanceFrom(nanobot: Nanobot): Int = distanceFrom(nanobot.pos)

    def distanceFrom(point: Point): Int = Point.distance(pos, point)
  }

  case class Nanobots(nanobots: Seq[Nanobot], bruteForce: Boolean = true) {
    lazy val strongest = nanobots.maxBy { _.r }

    def inRange(nanobot: Nanobot): Seq[Nanobot] =
      nanobots
        .filter { _.distanceFrom(nanobot) <= nanobot.r }

    def inRange(point: Point): Seq[Nanobot] =
      nanobots
        .filter { nanobot => nanobot.distanceFrom(point) <= nanobot.r }

    lazy val betterCoordinates: (Int, Set[Point]) =
      if (bruteForce) {
        val origin = Point(0, 0, 0)

        val MaxRange = nanobots
          .foldLeft(Int.MinValue) { (acc, nanobot) =>
            Math.max(acc, nanobot.distanceFrom(origin))
          }

        (
          for {
            x <- -MaxRange to +MaxRange
            y <- -MaxRange to +MaxRange
            z <- -MaxRange to +MaxRange
          } yield { Point(x, y, z) }
        )
          .map { point => (nanobots.filter { nanobot => nanobot.distanceFrom(point) <= nanobot.r }.size) -> point }
          .foldLeft((Int.MinValue, Set.empty[Point])) { (acc, info) =>
            if (acc._1 == info._1)
              acc._1 -> (acc._2 + info._2)
            else if (acc._1 < info._1) {
              info._1 -> Set(info._2)
            } else
                acc
          }
      } else {
        ???
      }
  }

  object Nanobots {
    final val re = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".r

    def parse(lines: Iterator[String]) =
      Nanobots(
        lines
          .map { line =>
            line match {
              case re(x, y, z, r) => Nanobot(Point(x.toInt, y.toInt, z.toInt), r.toInt)
            }
          }
          .toSeq
      )
  }

  def main(args: Array[String]): Unit = {
    val nanobots = Nanobots.parse(Source.fromResource("input-23.data").getLines())

    println(s"solution 1: ${nanobots.inRange(nanobots.strongest).size}")
  }
}
