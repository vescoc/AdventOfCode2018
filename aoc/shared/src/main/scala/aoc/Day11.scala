package aoc

import scala.annotation.tailrec
import scala.util.Try

object Day11 {
  val GridHeight = 300
  val GridWidth = 300

  def main(args: Array[String]): Unit = {
    val MaxSquareSize = Try {args(0).toInt } getOrElse GridWidth
    val OptMode = Try { args(1).toInt } getOrElse 0
    val GridSerialNumber = Try { args(2).toInt } getOrElse 5153

    val powerGrid = Array.tabulate(GridWidth, GridWidth) { (x, y) =>
      val X = x + 1
      val Y = y + 1

      val rankID = (X + 10)

      val value = (rankID * Y + GridSerialNumber) * rankID

      (value / 100) % 10 - 5
    }

    type Info = ((Int, Int, Int), Int)

    def solveBrutal(range: Range): Info = {
      (
        (
          for {
            d <- range
          } yield {
            val inner: Seq[Info] = (
              for {
                x <- 0 until GridWidth
                y <- 0 until GridHeight
              } yield {
                val cell: ((Int, Int, Int), Option[Int]) =
                  (
                    (x + 1, y + 1, d) ->
                      (
                        (
                          for {
                            dx <- x until x + d
                            dy <- y until y + d
                          } yield {
                            Try { Some(powerGrid(dx)(dy)) } getOrElse None
                          }
                        ).reduce { (oa: Option[Int], ob: Option[Int]) =>
                          for {
                            a <- oa
                            b <- ob
                          } yield (a + b)
                        }
                      )
                  )
                cell
              }
            ).collect { case (k @ _, Some(v @ _)) => (k, v) }

            inner
          }
        )
      )
        .flatten
        .maxBy { _._2 }
    }

    def solveOpt(range: Range): Info = {
      @tailrec
      def calc(current: Info, x: Int, y: Int, dimensions: Iterator[Int]): Info =
        if (y == GridHeight)
          current
        else if (dimensions.hasNext) {
          val dim = dimensions.next
          if (x + dim > GridWidth || y + dim > GridHeight)
            calc(current, x, y, dimensions)
          else {
            @tailrec
            def sum(s: Int, dx: Int, dy: Int): Int =
              if (dy == dim)
                s
              else {
                val ns = s + powerGrid(x + dx)(y + dy)
                val (ndx, ndy) = {
                  if (dx + 1 >= dim)
                    (0, dy + 1)
                  else
                    (dx + 1, dy)
                }
                sum(ns, ndx, ndy)
              }

            val candidate = sum(0, 0, 0)
            if (candidate > current._2)
              calc(((x + 1, y + 1, dim), candidate), x, y, dimensions)
            else
              calc(current, x, y, dimensions)
          }
        } else {
          val (nx, ny) = {
            if (x + 1 >= GridHeight) {
              val r = (0, y + 1)
              r
            } else
              (x + 1, y)
          }
          calc(current, nx, ny, range.iterator)
        }

      calc(((0, 0, 0), Int.MinValue), 0, 0, range.iterator)
    }

    def solveOpt2(range: Range): Info = {
      val max = powerGrid
        .map { _.max }
        .max

      val iterator = range.iterator

      @tailrec
      def calc(current: Info, x: Int, y: Int, dim: Int): Info =
        if (x + dim > GridWidth || y + dim > GridHeight) {
          if (x + 1 >= GridWidth) {
            if (y + 1 >= GridHeight) {
              if (iterator.hasNext) {
                val ndim = iterator.next

                calc(current, 0, 0, ndim)
              } else {
                current
              }
            } else {
              calc(current, 0, y + 1, dim)
            }
          } else {
            calc(current, x + 1, y, dim)
          }
        } else {
          @tailrec
          def sum(maxRemainder: Int, s: Int, dx: Int, dy: Int): Option[Int] =
            if (dy == dim) {
              Some(s)
            } else {
              val ns = s + powerGrid(x + dx)(y + dy)
              if (ns + maxRemainder < current._2)
                None
              else {
                val (ndx, ndy) = {
                  if (dx + 1 >= dim)
                    (0, dy + 1)
                  else
                    (dx + 1, dy)
                }

                sum(maxRemainder - max, ns, ndx, ndy)
              }
            }

          sum(max * dim * dim, 0, 0, 0) match {
            case Some(v) if v > current._2 =>
              calc(((x + 1, y + 1, dim), v), x + 1, y, dim)
            case _ =>
              calc(current, x + 1, y, dim)
          }
        }

      calc(((0, 0, 0), Int.MinValue), 0, 0, iterator.next)
    }
    
    val solve = List(solveOpt2 _, solveOpt _, solveBrutal _)(OptMode)

    val solution1 = solve(3 to 3)

    println(s"solution 1: $solution1")

    val solution2 = solve(1 to MaxSquareSize)

    println(s"solution 2: $solution2")
  }
}
