package aoc

object Day23 {
  case class Point(x: Int, y: Int, z: Int)
  object Point {
    def distance(a: Point, b: Point) =
      Math.abs(a.x - b.x) + Math.abs(a.y - b.y) + Math.abs(a.z - b.z)
  }

  case class Nanobot(pos: Point, r: Int) {
    def distanceFrom(nanobot: Nanobot) = Point.distance(nanobot.pos, pos)
  }

  case class Nanobots(nanobots: Seq[Nanobot]) {
    lazy val strongest = nanobots.maxBy { _.r }

    def inRange(nanobot: Nanobot) =
      nanobots
        .filter { _.distanceFrom(nanobot) <= nanobot.r }
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
