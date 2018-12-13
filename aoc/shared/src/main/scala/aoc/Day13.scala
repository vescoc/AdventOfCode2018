package aoc

import scala.annotation.tailrec
import scala.util.Try

object Day13 {
  case class Point(x: Int, y: Int)

  object Turn extends Enumeration {
    type Turn = Value

    val Left, Straight, Right = Value
  }

  object Direction extends Enumeration {
    type Direction = Char

    val Up = '^'
    val Down = 'V'
    val Left = '<'
    val Right = '>'
  }

  object Track extends Enumeration {
    type Track = Char

    val Vertical = '|'
    val Horizontal = '-'
    val TurnLeft = '\\'
    val TurnRight = '/'
    val Intersection = '+'
  }

  import Turn._
  import Direction._
  import Track._

  case class Cart(id: Int, location: Point, direction: Direction, turn: Turn = Turn.Left) {
    def move(c: Track): Cart = {
      val t =
        (c, turn) match {
          case (Track.Intersection, Turn.Left) =>
            Turn.Straight
          case (Track.Intersection, Turn.Straight) =>
            Turn.Right
          case (Track.Intersection, Turn.Right) =>
            Turn.Left
          case _ =>
            turn
        }

      val l = {
        val (dx, dy) = direction match {
          case Direction.Up | Direction.Down =>
            val d =
              if (direction == Direction.Up)
                -1
              else
                +1

            if (c == Track.Vertical || c == Track.Intersection && turn == Turn.Straight)
              (0, d)
            else if (c == Track.TurnLeft || c == Track.Intersection && turn == Turn.Left)
              (d, 0)
            else if (c == Track.TurnRight || c == Track.Intersection && turn == Turn.Right)
              (-d, 0)
            else
              (0, 0)

          case Direction.Left | Direction.Right =>
            val d =
              if (direction == Direction.Left)
                -1
              else
                +1

            if (c == Track.Horizontal || c == Track.Intersection && turn == Turn.Straight)
              (d, 0)
            else if (c == Track.TurnLeft || c == Track.Intersection && turn == Turn.Right)
              (0, d)
            else if (c == Track.TurnRight || c == Track.Intersection && turn == Turn.Left)
              (0, -d)
            else
              (0, 0)
        }

        if (dx == 0 && dy == 0)
          sys.error(s"invalid command: '$c' direction: $direction turn: $turn")

        location.copy(location.x + dx, location.y + dy)
      }

      val d = {
        if (c == Track.Vertical || c == Track.Horizontal || (c == Track.Intersection && turn == Turn.Straight))
          direction
        else if (direction == Direction.Up && (c == Track.TurnLeft || c == Track.Intersection && turn == Turn.Left)
                 || direction == Direction.Down && (c == Track.TurnRight || c == Track.Intersection && turn == Turn.Right))
          Direction.Left
        else if (direction == Direction.Left && (c == Track.TurnLeft || c == Track.Intersection && turn == Turn.Right)
                 || direction == Direction.Right && (c == Track.TurnRight || c == Track.Intersection && turn == Turn.Left))
          Direction.Up
        else if (direction == Direction.Up && (c == Track.TurnRight || c == Track.Intersection && turn == Turn.Right)
                 || direction == Direction.Down && (c == Track.TurnLeft || c == Track.Intersection && turn == Turn.Left))
          Direction.Right
        else
          Direction.Down
      }

      copy(location = l, direction = d, turn = t)
    }
  }

  case class Problem(lines: Iterator[String]) {
    lazy val (tracks, carts) = {
      val id = Iterator.from(0)

      lines.zipWithIndex
        .map { p =>
          val (l, y) = p

          l.toUpperCase.zipWithIndex
            .map { p =>
              val (c, x) = p

              c match {
                case Direction.Up | Direction.Down =>
                  (Track.Vertical, Some(Cart(id.next, Point(x, y), c)))
                case Direction.Left | Direction.Right =>
                  (Track.Horizontal, Some(Cart(id.next, Point(x, y), c)))
                case _ =>
                  (c, None)
              }
            }
            .foldLeft(("", List.empty[Cart])) { (acc, value) =>
              (acc._1 + value._1, acc._2 ++ value._2.toList)
            }
        }
        .foldLeft((List.empty[String], List.empty[Cart])) { (acc, value) =>
          (acc._1 ++ List(value._1), acc._2 ++ value._2)
        }
    }

    lazy val TrackHeight = tracks.size
    lazy val TrackWidth = tracks(0).size

    def tick(carts: List[Cart]): (List[Cart], List[Point]) = {
      @tailrec
      def tick(
        carts: List[Cart],
        done: List[Cart] = List.empty,
        collisions: List[Point] = List.empty
      ): (List[Cart], List[Point]) =
        carts match {
          case h :: hs =>
            val newDone = h.move(tracks(h.location.y)(h.location.x)) :: done

            val crashed = (hs ++ newDone)
              .groupBy { _.location }
              .filter { _._2.size > 1 }
              .map { p =>
                (p._1, p._2)
              }

            val crashedSet = crashed.flatMap { _._2 }.toSet

            tick(
              hs.filterNot { crashedSet.contains(_) },
              newDone.filterNot { crashedSet.contains(_) },
              collisions ++ crashed.map { _._1 }
            )
          case _ =>
            (done, collisions)
        }

      tick(
        carts
          .sortBy { cart =>
            cart.location.y * TrackHeight + cart.location.x
          }
      )
    }

    @tailrec
    final def collision(count: Int = 0, carts: List[Cart] = carts): (Int, Point, List[Cart]) = {
      val (newCarts, collisions) = tick(carts)

      collisions match {
        case h :: _ =>
          (count, h, newCarts)
        case _ =>
          collision(count + 1, newCarts)
      }
    }

    @tailrec
    final def reduce(count: Int = 0, carts: List[Cart] = carts): (Int, Option[Point]) =
      if (carts.size < 2)
        (count, Try { Some(carts(0).location) } getOrElse None)
      else {
        val (newCount, h, newCarts) = collision(count, carts)

        reduce(newCount, newCarts)
      }

    def solution1() = collision()._2

    def solution2() = reduce()._2
  }

  def main(args: Array[String]): Unit = {
    val problem = Problem(
      Source
        .fromResource("input-13.data")
        .getLines()
    )

    println(s"solution 1: ${problem.solution1()}")
    println(s"solution 2: ${problem.solution2()}")
  }
}
