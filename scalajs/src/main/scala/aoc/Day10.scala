package aoc

object Day10 {
  val re = """position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>""".r

  def main(args: Array[String]): Unit = {
    val input = Input10
      .content
      .lines
      .map {
        _ match {
          case re(x, y, vx, vy) => ((x, y), (vx, vy))
        }
      }
      .toList

    println(input)
  }
}
