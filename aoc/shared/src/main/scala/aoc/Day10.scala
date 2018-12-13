package aoc

object Day10 {
  val SmallDelta = 1
  val BigDelta = 100

  val re = """position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>""".r

  case class Point(x: Int, y: Int)
  case class Velocity(dx: Int, dy: Int)

  case class Star(p: Point, v: Velocity) {
    def time(t: Int): Star =
      copy(p = Point(p.x + v.dx * t, p.y + v.dy * t))
  }

  val input = Source
    .fromResource("input-10.data")
    .getLines()
    .map {
      _ match {
        case re(x, y, vx, vy) =>
          Star(Point(x.toInt, y.toInt), Velocity(vx.toInt, vy.toInt))
      }
    }
    .toList
}
