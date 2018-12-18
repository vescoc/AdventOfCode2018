package aoc

import scala.collection.mutable

object Day17 {
  val xre = """x=(\d+), y=(\d+)..(\d+)""".r
  val yre = """y=(\d+), x=(\d+)..(\d+)""".r

  object GroundElement extends Enumeration {
    type GroundElement = String

    val PrimarySource = "+"
    val Clay = "#"
    val Sand = "."
    val Water = "~"
    val AntMark = "@"
  }

  import GroundElement._

  object State extends Enumeration {
    type State = Value

    val Done, Down, Wait, GoLeft, GoRight, ClayFound, Up, FallFound, WaitChild = Value
  }

  import State._

  type Info = ((Int, Int)) => GroundElement

  case class Ground(ground: Map[(Int, Int), GroundElement]) {
    def apply(x: Int, y: Int) =
      ground.getOrElse(
        (x, y),
        if (x == 500 && y == 0) PrimarySource else Sand
      )

    private implicit class MinPoint(point: (Int, Int)) {
      def min(that: (Int, Int)) = (Math.min(point._1, that._1), Math.min(point._2, that._2))
    }

    private implicit class MaxPoint(point: (Int, Int)) {
      def max(that: (Int, Int)) = (Math.max(point._1, that._1), Math.max(point._2, that._2))
    }

    lazy val (min, max) = ground
      .foldLeft(((Int.MaxValue, Int.MaxValue), (Int.MinValue, Int.MinValue))) { (acc, value) =>
        (acc._1 min value._1, acc._2 max value._1)
      }

    override def toString =
      (
        for {
          y <- (min._2 - 1) to (max._2 + 1)
        } yield {
          (
            for {
              x <- (min._1 - 1) to (max._1 + 1)
            } yield { apply(x, y) }
          )
            .mkString("")
        }
      )
        .mkString("\n")

    def ant() = new Ant(p => apply(p._1, p._2))

    class Ant(info: Info) {
      val set: mutable.Set[(Int, Int)] = mutable.Set()

      var state: State = Down

      var fatherAnt: Ant = null
      var leftAnt: Ant = null
      var rightAnt: Ant = null
      var downAnt: Ant = null

      var position: (Int, Int) = (500, 0)

      def apply(position: (Int, Int) = position): GroundElement =
        if (position == this.position)
          AntMark
        else if (set.contains(position))
          Water
        else
          info(position)

      def walk(): Unit =
        state match {
          case Wait =>
            leftAnt.walk()
            rightAnt.walk()

            if (leftAnt.state == ClayFound && rightAnt.state == ClayFound) {
              set ++= leftAnt.set
              set ++= rightAnt.set

              leftAnt = null
              rightAnt = null

              state = Up
            } else if (leftAnt.state == Done && rightAnt.state == Done) {
              set ++= leftAnt.set
              set ++= rightAnt.set

              leftAnt = null
              rightAnt = null

              state = Done
            }

          case WaitChild =>
            downAnt.walk()

            if (downAnt.state == Done) {
              set ++= downAnt.set
              downAnt = null

              state = Done
            }

          case GoLeft =>
            val np = (position._1 - 1, position._2)
            apply(np) match {
              case Clay =>
                state = ClayFound
              case _ =>
                position = np
                set += np

                val up = (position._1, position._2 + 1)
                if (apply(up) == Sand)
                  state = FallFound
            }

          case ClayFound =>

          case Up =>
            position = (position._1, position._2 - 1)
            state = Wait

            leftAnt = {
              val ant = new Ant(apply)

              ant.fatherAnt = this
              ant.state = GoLeft
              ant.position = position

              ant
            }

            rightAnt = {
              val ant = new Ant(apply)

              ant.fatherAnt = this
              ant.state = GoRight
              ant.position = position

              ant
            }

          case GoRight =>
            val np = (position._1 + 1, position._2)
            apply(np) match {
              case Clay =>
                state = ClayFound
              case _ =>
                position = np
                set += np

                val up = (position._1, position._2 + 1)
                if (apply(up) == Sand)
                  state = FallFound
            }

          case FallFound =>
            state = WaitChild

            downAnt = {
              val ant = new Ant(apply)

              ant.fatherAnt = this
              ant.state = Down
              ant.position = position

              ant
            }

          case Down =>
            val np = (position._1, position._2 + 1)
            if (np._2 > max._2)
              state = Done
            else {
              apply(np) match {
                case Clay =>
                  state = Wait

                  leftAnt = {
                    val ant = new Ant(apply)

                    ant.fatherAnt = this
                    ant.state = GoLeft
                    ant.position = position

                    ant
                  }

                  rightAnt = {
                    val ant = new Ant(apply)

                    ant.fatherAnt = this
                    ant.state = GoRight
                    ant.position = position

                    ant
                  }
                case _ =>
                  position = np
                  set += np
              }
            }

          case Done =>
        }

      override def toString =
        (
          List(
            (
              for {
                y <- (min._2 - 1) to (max._2 + 1)
              } yield {
                (
                  for {
                    x <- (min._1 - 1) to (max._1 + 1)
                  } yield { apply((x, y)) }
                )
                  .mkString("")
              }
            )
              .mkString("\n")
          ) ++ (
            if (leftAnt != null)
              List(leftAnt.toString)
            else
              List.empty[String]
          ) ++ (
            if (rightAnt != null)
              List(rightAnt.toString)
            else
              List.empty[String]
          ) ++ (
            if (downAnt != null)
              List(downAnt.toString)
            else
              List()
          )
        ).mkString("\n--------------\n")
    }
  }

  def parse(lines: Iterator[String]): Ground =
    Ground(
      lines
        .foldLeft(Map.empty[(Int, Int), GroundElement]) { (acc, line) =>
          acc ++ (
            line match {
              case xre(x, sy, ey) =>
                val (ix, isy, iey) = (x.toInt, sy.toInt, ey.toInt)
                for {
                  y <- isy to iey
                } yield { (ix, y) -> Clay }
              case yre(y, sx, ex) =>
                val (iy, isx, iex) = (y.toInt, sx.toInt, ex.toInt)
                for {
                  x <- isx to iex
                } yield { (x, iy) -> Clay }
            }
          )
        }
    )

  def main(args: Array[String]): Unit = {
  }
}
