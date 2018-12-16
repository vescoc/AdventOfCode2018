package aoc

import org.scalatest.{MustMatchers, WordSpec}

import aoc.Day15._

class Day15Spec extends WordSpec with MustMatchers {
  def minion(state: State, id: Int) = state.getById(id)

  def round(state: State, count: Int) =
    (1 to count).foldLeft(state) { (acc, _) =>
      acc.round()
    }

  "round test big" must {
    val state = Dungeon.parse(
      s"""|#########
          |#G..G..G#
          |#.......#
          |#.......#
          |#G..E..G#
          |#.......#
          |#.......#
          |#G..G..G#
          |#########""".stripMargin.lines
    )

    "equals to example after 1 round" in {
      val s = round(state, 1)

      minion(s, 0) mustBe (Minion('G', 0, Point(2, 1)))
      minion(s, 1) mustBe (Minion('G', 1, Point(4, 2), 197))
      minion(s, 2) mustBe (Minion('G', 2, Point(6, 1)))
      minion(s, 3) mustBe (Minion('G', 3, Point(2, 4)))
      minion(s, 4) mustBe (Minion('E', 4, Point(4, 3)))
      minion(s, 5) mustBe (Minion('G', 5, Point(7, 3)))
    }

    "equals to example after 2 round" in {
      val s = round(state, 2)

      println("test big after 2")
      println(s)

      minion(s, 0) mustBe (Minion('G', 0, Point(3, 1)))
      minion(s, 1) mustBe (Minion('G', 1, Point(4, 2), 194))
      minion(s, 2) mustBe (Minion('G', 2, Point(5, 1)))
      minion(s, 3) mustBe (Minion('G', 3, Point(2, 3)))
      minion(s, 4) mustBe (Minion('E', 4, Point(4, 3), 197))
      minion(s, 5) mustBe (Minion('G', 5, Point(6, 3)))
      minion(s, 6) mustBe (Minion('G', 6, Point(1, 5)))
    }

    "equals to example after 3 round" in {
      val s = round(state, 3)

      println("test big after 3")
      println(s)

      minion(s, 0) mustBe (Minion('G', 0, Point(3, 2)))
      minion(s, 1) mustBe (Minion('G', 1, Point(4, 2), 191))
      minion(s, 2) mustBe (Minion('G', 2, Point(5, 2)))
      minion(s, 3) mustBe (Minion('G', 3, Point(3, 3)))
      minion(s, 4) mustBe (Minion('E', 4, Point(4, 3), 185))
      minion(s, 5) mustBe (Minion('G', 5, Point(5, 3)))
      minion(s, 6) mustBe (Minion('G', 6, Point(1, 4)))
    }
  }

  "round test small" must {
    "equals to example" in {
      val state = Dungeon
        .parse(
          s"""|#######
              |#.E...#
              |#.....#
              |#...G.#
              |#######""".stripMargin.lines
        )
        .round()

      minion(state, 0) mustBe (Minion('E', 0, Point(3, 1)))
      minion(state, 1) mustBe (Minion('G', 1, Point(4, 2)))
    }
  }

  "round test fight" must {
    val state = Dungeon.parse(
      """|#######
         |#.G...#
         |#...EG#
         |#.#.#G#
         |#..G#E#
         |#.....#
         |#######""".stripMargin.lines
    )

    "equals to example round 1" in {
      val s = round(state, 1)

      println("fight round 1")
      println(s)

      minion(s, 0) mustBe (Minion('G', 0, Point(3, 1)))
      minion(s, 1) mustBe (Minion('E', 1, Point(4, 2), 197))
      minion(s, 2) mustBe (Minion('G', 2, Point(5, 2), 197))
      minion(s, 3) mustBe (Minion('G', 3, Point(5, 3), 197))
      minion(s, 4) mustBe (Minion('G', 4, Point(3, 3)))
      minion(s, 5) mustBe (Minion('E', 5, Point(5, 4), 197))
    }

    "equals to example round 2" in {
      val s = round(state, 2)

      println("fight round 2")
      println(s)

      minion(s, 0) mustBe (Minion('G', 0, Point(4, 1)))
      minion(s, 1) mustBe (Minion('E', 1, Point(4, 2), 188))
      minion(s, 2) mustBe (Minion('G', 2, Point(5, 2), 194))
      minion(s, 3) mustBe (Minion('G', 3, Point(5, 3), 194))
      minion(s, 4) mustBe (Minion('G', 4, Point(3, 2)))
      minion(s, 5) mustBe (Minion('E', 5, Point(5, 4), 194))
    }

    "equals to example round 23" in {
      val s = round(state, 23)

      println("fight round 23")
      println(s)

      minion(s, 0) mustBe (Minion('G', 0, Point(4, 1)))
      minion(s, 1) mustBe (Minion('E', 1, Point(4, 2), -1))
      minion(s, 2) mustBe (Minion('G', 2, Point(5, 2), 131))
      minion(s, 3) mustBe (Minion('G', 3, Point(5, 3), 131))
      minion(s, 4) mustBe (Minion('G', 4, Point(3, 2)))
      minion(s, 5) mustBe (Minion('E', 5, Point(5, 4), 131))
    }

    "equals to example round 24" in {
      val s = round(state, 24)

      println("fight round 24")
      println(s)

      minion(s, 0) mustBe (Minion('G', 0, Point(3, 1)))
      minion(s, 1) mustBe (Minion('E', 1, Point(4, 2), -1))
      minion(s, 2) mustBe (Minion('G', 2, Point(4, 2), 131))
      minion(s, 3) mustBe (Minion('G', 3, Point(5, 3), 128))
      minion(s, 4) mustBe (Minion('G', 4, Point(3, 3)))
      minion(s, 5) mustBe (Minion('E', 5, Point(5, 4), 128))
    }

    "equals to example round 25" in {
      val s = round(state, 25)

      println("fight round 25")
      println(s)

      minion(s, 0) mustBe (Minion('G', 0, Point(2, 1)))
      minion(s, 1) mustBe (Minion('E', 1, Point(4, 2), -1))
      minion(s, 2) mustBe (Minion('G', 2, Point(3, 2), 131))
      minion(s, 3) mustBe (Minion('G', 3, Point(5, 3), 125))
      minion(s, 4) mustBe (Minion('G', 4, Point(3, 4)))
      minion(s, 5) mustBe (Minion('E', 5, Point(5, 4), 125))
    }

    "equals to example round 26" in {
      val s = round(state, 26)

      println("fight round 26")
      println(s)

      minion(s, 0) mustBe (Minion('G', 0, Point(1, 1)))
      minion(s, 1) mustBe (Minion('E', 1, Point(4, 2), -1))
      minion(s, 2) mustBe (Minion('G', 2, Point(2, 2), 131))
      minion(s, 3) mustBe (Minion('G', 3, Point(5, 3), 122))
      minion(s, 4) mustBe (Minion('G', 4, Point(3, 5)))
      minion(s, 5) mustBe (Minion('E', 5, Point(5, 4), 122))
    }

    "equals to example round 27" in {
      val s = round(state, 27)

      println("fight round 27")
      println(s)

      minion(s, 0) mustBe (Minion('G', 0, Point(1, 1)))
      minion(s, 1) mustBe (Minion('E', 1, Point(4, 2), -1))
      minion(s, 2) mustBe (Minion('G', 2, Point(2, 2), 131))
      minion(s, 3) mustBe (Minion('G', 3, Point(5, 3), 119))
      minion(s, 4) mustBe (Minion('G', 4, Point(4, 5)))
      minion(s, 5) mustBe (Minion('E', 5, Point(5, 4), 119))
    }

    "equals to example round 28" in {
      val s = round(state, 28)

      println("fight round 28")
      println(s)

      minion(s, 0) mustBe (Minion('G', 0, Point(1, 1)))
      minion(s, 1) mustBe (Minion('E', 1, Point(4, 2), -1))
      minion(s, 2) mustBe (Minion('G', 2, Point(2, 2), 131))
      minion(s, 3) mustBe (Minion('G', 3, Point(5, 3), 116))
      minion(s, 4) mustBe (Minion('G', 4, Point(5, 5)))
      minion(s, 5) mustBe (Minion('E', 5, Point(5, 4), 113))
    }

    "equals to example round 47" in {
      val s = round(state, 47)

      println("fight round 47")
      println(s)

      minion(s, 0) mustBe (Minion('G', 0, Point(1, 1)))
      minion(s, 1) mustBe (Minion('E', 1, Point(4, 2), -1))
      minion(s, 2) mustBe (Minion('G', 2, Point(2, 2), 131))
      minion(s, 3) mustBe (Minion('G', 3, Point(5, 3), 59))
      minion(s, 4) mustBe (Minion('G', 4, Point(5, 5)))
      minion(s, 5) mustBe (Minion('E', 5, Point(5, 4), -1))

      println(s"score: ${s.score. map { p => (p._1, (p._2, p._2 * 47)) }}")
    }
  }

  "fight example 1" must {
    "end to 37" in {
      val state = Dungeon.parse(
        """|#######
           |#G..#E#
           |#E#E.E#
           |#G.##.#
           |#...#E#
           |#...E.#
           |#######"""
          .stripMargin
          .lines
      )

      val s = round(state, 37)

      println("fight 1 round 37")
      println(s)

      minion(s, 0) mustBe (Minion('G', 0, Point(1, 1), -1))
      minion(s, 1) mustBe (Minion('E', 1, Point(5, 1)))
      minion(s, 3) mustBe (Minion('E', 3, Point(1, 2), 197))
    }
  }

  "solution 1 test" must {
    "equals 36334" in {
      val (score, state) =
        solution1(
          s"""|#######
              |#G..#E#
              |#E#E.E#
              |#G.##.#
              |#...#E#
              |#...E.#
              |#######""".stripMargin.lines
        )

      score mustBe (36334)
    }
  }
}
