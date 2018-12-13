package aoc

import org.scalatest.{MustMatchers, WordSpec}

import Day13._

class Day13Spec extends WordSpec with MustMatchers {
  "A cart" must {
    "go down on vertical from direction down" in {
      val cart = Cart(Point(1, 1), Direction.Down)

      val newCart = cart.move(Track.Vertical)

      newCart.location mustBe (Point(1, 2))
      newCart.direction mustBe (cart.direction)
      newCart.turn mustBe (cart.turn)
    }

    "go up on vertical from direction up" in {
      val cart = Cart(Point(1, 1), Direction.Up)

      val newCart = cart.move(Track.Vertical)

      newCart.location mustBe (Point(1, 0))
      newCart.direction mustBe (cart.direction)
      newCart.turn mustBe (cart.turn)
    }

    "go left on horizontal from direction left" in {
      val cart = Cart(Point(1, 1), Direction.Left)

      val newCart = cart.move(Track.Horizontal)

      newCart.location mustBe (Point(0, 1))
      newCart.direction mustBe (cart.direction)
      newCart.turn mustBe (cart.turn)
    }

    "go right on horizontal from direction right" in {
      val cart = Cart(Point(1, 1), Direction.Right)

      val newCart = cart.move(Track.Horizontal)

      newCart.location mustBe (Point(2, 1))
      newCart.direction mustBe (cart.direction)
      newCart.turn mustBe (cart.turn)
    }

    "go down on turn left from direction right" in {
      val cart = Cart(Point(1, 1), Direction.Right)

      val newCart = cart.move(Track.TurnLeft)

      newCart.location mustBe (Point(1, 2))
      newCart.direction mustBe (Direction.Down)
      newCart.turn mustBe (cart.turn)
    }

    "go up on turn left from direction left" in {
      val cart = Cart(Point(1, 1), Direction.Left)

      val newCart = cart.move(Track.TurnLeft)

      newCart.location mustBe (Point(1, 0))
      newCart.direction mustBe (Direction.Up)
      newCart.turn mustBe (cart.turn)
    }

    "go up on turn right from direction right" in {
      val cart = Cart(Point(1, 1), Direction.Right)

      val newCart = cart.move(Track.TurnRight)

      newCart.location mustBe (Point(1, 0))
      newCart.direction mustBe (Direction.Up)
      newCart.turn mustBe (cart.turn)
    }

    "go down on turn right from direction left" in {
      val cart = Cart(Point(1, 1), Direction.Left)

      val newCart = cart.move(Track.TurnRight)

      newCart.location mustBe (Point(1, 2))
      newCart.direction mustBe (Direction.Down)
      newCart.turn mustBe (cart.turn)
    }

    "go up on intersection from direction right next turn left" in {
      val cart = Cart(Point(1, 1), Direction.Right, Turn.Left)

      val newCart = cart.move(Track.Intersection)

      newCart.location mustBe (Point(1, 0))
      newCart.direction mustBe (Direction.Up)
      newCart.turn mustBe (Turn.Straight)
    }

    "go up on intersection from direction left next turn right" in {
      val cart = Cart(Point(1, 1), Direction.Left, Turn.Right)

      val newCart = cart.move(Track.Intersection)

      newCart.location mustBe (Point(1, 0))
      newCart.direction mustBe (Direction.Up)
      newCart.turn mustBe (Turn.Left)
    }

    "go down on intersection from direction right next turn right" in {
      val cart = Cart(Point(1, 1), Direction.Right, Turn.Right)

      val newCart = cart.move(Track.Intersection)

      newCart.location mustBe (Point(1, 2))
      newCart.direction mustBe (Direction.Down)
      newCart.turn mustBe (Turn.Left)
    }

    "go down on intersection from direction left next turn left" in {
      val cart = Cart(Point(1, 1), Direction.Left, Turn.Left)

      val newCart = cart.move(Track.Intersection)

      newCart.location mustBe (Point(1, 2))
      newCart.direction mustBe (Direction.Down)
      newCart.turn mustBe (Turn.Straight)
    }

    "go right on intersection from direction up next turn right" in {
      val cart = Cart(Point(1, 1), Direction.Up, Turn.Right)

      val newCart = cart.move(Track.Intersection)

      newCart.location mustBe (Point(2, 1))
      newCart.direction mustBe (Direction.Right)
      newCart.turn mustBe (Turn.Left)
    }

    "go right on intersection from direction down next turn left" in {
      val cart = Cart(Point(1, 1), Direction.Down, Turn.Left)

      val newCart = cart.move(Track.Intersection)

      newCart.location mustBe (Point(2, 1))
      newCart.direction mustBe (Direction.Right)
      newCart.turn mustBe (Turn.Straight)
    }

    "go left on intersection from direction up next turn left" in {
      val cart = Cart(Point(1, 1), Direction.Up, Turn.Left)

      val newCart = cart.move(Track.Intersection)

      newCart.location mustBe (Point(0, 1))
      newCart.direction mustBe (Direction.Left)
      newCart.turn mustBe (Turn.Straight)
    }

    "go left on intersection from direction down next turn right" in {
      val cart = Cart(Point(1, 1), Direction.Down, Turn.Right)

      val newCart = cart.move(Track.Intersection)

      newCart.location mustBe (Point(0, 1))
      newCart.direction mustBe (Direction.Left)
      newCart.turn mustBe (Turn.Left)
    }
  }

  "The test part1 problem" must {
    "have a good solution1" in {
      Problem(
        """|/->-\
           ||   |  /----\
           || /-+--+-\  |
           || | |  | v  |
           |\-+-/  \-+--/
           |  \------/   """.stripMargin.lines
      ).solution1 mustBe (Point(7, 3))
    }
  }

  "The test part2 problem" must {
    "have a good solution2" in {
      Problem(
        """|/>-<\  
           ||   |  
           || /<+-\
           || | | v
           |\>+</ |
           |  |   ^
           |  \<->/""".stripMargin.lines
      ).solution2 mustBe (Some(Point(6, 4)))
    }
  }
}
