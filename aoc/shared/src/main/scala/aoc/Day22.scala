package aoc

object Day22 {
  val depthRe = """depth: (\d+)""".r
  val target= """target: (\d+),(\d+)""".r

  object MazeType extends Enumeration {
    type MazeType = Int

    val Rocky = 0
    val Wet = 1
    val Narrow = 2
  }

  import MazeType._

  case class Maze(depth: Int, target: (Int, Int)) {
    private lazy val map = {
      val arr = Array.ofDim[(Long, Long, MazeType)](target._1 + 1, target._2 + 1)

      for {
        y <- 0 to target._2
        x <- 0 to target._1
      } {
        val geologicalIndex = (x, y) match {
          case (0, 0) | `target` => 0
          case (0, _) => y * 48271L
          case (_, 0) => x * 16807L
          case (_, _) => arr(x - 1)(y)._2 * arr(x)(y - 1)._2
        }

        val erosionLevel = (geologicalIndex + depth) % 20183
        val mazeType = (erosionLevel % 3).toInt

        arr(x)(y) = (geologicalIndex, erosionLevel, mazeType)
      }

      arr
    }

    trait Info {
      def geologicalIndex: Long
      def erosionLevel: Long
      def mazeType: Int
    }

    def apply(x: Int, y: Int): Info = {
      val v = map(x)(y)

      new Info {
        val geologicalIndex = v._1
        val erosionLevel = v._2
        val mazeType = v._3
      }
    }

    lazy val riskLevel =
      (
        for {
          x <- 0 to target._1
          y <- 0 to target._2
        } yield { map(x)(y)._3 }
      )
        .sum
  }

  def parse(data: String) = {
    val maze = data
      .lines
      .foldLeft[(Option[Int], Option[(Int, Int)])]((None, None)) { (acc, line) =>
        (acc, line) match {
          case ((None, _), depthRe(depth)) =>
            (Some(depth.toInt), acc._2)
          case ((_, None), target(x, y)) =>
            (acc._1, Some((x.toInt, y.toInt)))
        }
      }

    Maze(maze._1.get, maze._2.get)
  }    

  def main(args: Array[String]): Unit = {
    val maze = parse(Source.fromResource("input-22.data").mkString)

    println(s"solution 1: ${maze.riskLevel}")
  }
}
