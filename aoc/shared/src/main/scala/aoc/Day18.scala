package aoc

import scala.annotation.tailrec

object Day18 {
  object LandscapeType extends Enumeration {
    type LandscapeType = Char

    val OpenGround = '.'
    val Trees = '|'
    val Lumberyard = '#'
  }

  import LandscapeType._

  case class Landscape(data: List[String]) {
    lazy val Width = data(0).size
    lazy val Height = data.size

    def apply(x: Int, y: Int) =
      if (x >= 0 && x < Width
          && y >= 0 && y < Height)
        data(y)(x)
      else
        OpenGround

    def evolve(): Landscape =
      Landscape(
        (
          for {
            y <- 0 until Height
          } yield {
            (
              for {
                x <- 0 until Width
              } yield {
                val info =
                  (
                    for {
                      dy <- -1 to 1
                      dx <- -1 to 1
                      if dx != 0 || dy != 0
                      cx = x + dx
                      cy = y + dy
                    } yield { apply(cx, cy) }
                  ).groupBy { c =>
                      c
                    }
                    .map { p =>
                      p._1 -> p._2.size
                    }

                val c = apply(x, y)

                val r = c match {
                  case OpenGround if info.getOrElse(Trees, 0) >= 3 =>
                    Trees
                  case Trees if info.getOrElse(Lumberyard, 0) >= 3 =>
                    Lumberyard
                  case Lumberyard if !(info.getOrElse(Lumberyard, 0) >= 1 && info.getOrElse(Trees, 0) >= 1) =>
                    OpenGround
                  case c @ _ =>
                    c
                }

                r
              }
            ).mkString
          }
        ).toList
      )

    override def toString = data.mkString("\n")

    def resourceValue = {
      val info = data
        .flatMap { row =>
          row.map { c =>
            c
          }
        }
        .groupBy { c =>
          c
        }
        .map { p =>
          p._1 -> p._2.size
        }

      info.getOrElse(Trees, 0).toLong * info.getOrElse(Lumberyard, 0).toLong
    }

    def evolve(count: Long): Landscape = {
      @tailrec
      def helper(count: Long = count, current: Landscape = this, story: List[Landscape] = List.empty): Landscape =
        if (count == 0)
          current
        else {
          val landscape = current.evolve()

          val cycle = story.indexOf(landscape)
          if (cycle != -1) {
            landscape.evolveRaw(
              (count - 1) % (cycle + 1)
            )
          } else {
            helper(count - 1, landscape, landscape :: story)
          }
        }

      helper()
    }

    def evolveRaw(count: Long): Landscape = {
      @tailrec
      def helper(count: Long = count, current: Landscape = this): Landscape =
        if (count == 0)
          current
        else {
          val landscape = current.evolve()

          helper(count - 1, landscape)
        }

      helper()
    }
  }

  def main(args: Array[String]): Unit = {
    val landscape = Landscape(
      Source
        .fromResource("input-18.data")
        .getLines()
        .toList
    )

    val solution1 = landscape.evolve(10).resourceValue

    println(s"solution 1: $solution1")

    val solution2 = landscape.evolve(1000000000L).resourceValue

    println(s"solution 2: $solution2")
  }
}
