package aoc

object Day13 {
  case class Point(x: Int, y: Int)

  object Turn extends Enumeration {
    type Turn = Value

    val Left, Straigh, Right = Value
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

  case class Cart(location: Point, direction: Direction, lastTurn: Turn = Turn.Right) {
    def move(c: Track): Cart = {
      c match {
        case Track.Vertical if direction == Direction.Up =>
          copy(location = location.copy(y = location.y - 1))
        case Track.Vertical if direction == Direction.Down =>
          copy(location = location.copy(y = location.y + 1))
        case Track.Horizontal if direction == Direction.Left =>
          copy(location = location.copy(x = location.x - 1))
        case Track.Horizontal if direction == Direction.Right =>
          copy(location = location.copy(x = location.x + 1))
        case Track.TurnLeft if direction == Direction.Up =>
          copy(location = location.copy(x = location.x - 1), direction = Direction.Left)
        case Track.TurnLeft if direction == Direction.Down =>
          copy(location = location.copy(x = location.x + 1), direction = Direction.Right)
        case Track.TurnLeft if direction == Direction.Left =>
          copy(location = location.copy(y = location.y - 1), direction = Direction.Up)
        case Track.TurnLeft if direction == Direction.Right =>
          copy(location = location.copy(y = location.y + 1), direction = Direction.Down)
        case Track.TurnRight if direction == Direction.Up =>
          copy(location = location.copy(x = location.x + 1), direction = Direction.Right)
        case Track.TurnRight if direction == Direction.Down =>
          copy(location = location.copy(x = location.x - 1), direction = Direction.Left)
        case Track.TurnRight if direction == Direction.Left =>
          copy(location = location.copy(x = location.y + 1), direction = Direction.Down)
        case Track.TurnRight if direction == Direction.Right =>
          copy(location = location.copy(x = location.y - 1), direction = Direction.Up)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val (tracks, carts) = Source
      .fromResource("input-13.data")
      .getLines()
      .zipWithIndex
      .map { p =>
        val (l, y) = p

        l
          .zipWithIndex
          .map { p =>
            val (c, x) = p

            c match {
              case Direction.Up | Direction.Down =>
                (Track.Vertical, Some(Cart(Point(x, y), c)))
              case Direction.Left | Direction.Right =>
                (Track.Horizontal, Some(Cart(Point(x, y), c)))
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

    val TrackHeight = tracks.size

    def tick(carts: List[Cart] = carts): List[Cart] = {
      def tick(cart: Cart): Cart = {
        val c = tracks(cart.location.y)(cart.location.x)

        cart.move(c)
      }

      carts
        .sortBy { cart =>
          cart.location.y * TrackHeight + cart.location.x
        }
        .map { tick }
    }


    println(carts)
    println(tick())
  }
}
