package aoc

import org.scalatest.{MustMatchers, WordSpec}

import aoc.Day15._

class Day15Spec extends WordSpec with MustMatchers {
  def minion(state: State, id: Int) =
    state.minions.filter { _.id == id }.head

  def round(state: State, count: Int) =
    (1 to count).foldLeft(state) { (acc, _) => acc.round() }

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
          |#########"""
        .stripMargin
        .lines
    )

    "equals to example after 1 round" in {
      val s = round(state, 1)

      minion(s, 0) mustBe (Minion('G', 0, Point(2, 1)))
      minion(s, 1) mustBe (Minion('G', 1, Point(4, 2)))
      minion(s, 2) mustBe (Minion('G', 2, Point(6, 1)))
      minion(s, 3) mustBe (Minion('G', 3, Point(2, 4)))
      minion(s, 4) mustBe (Minion('E', 4, Point(4, 3)))
      minion(s, 5) mustBe (Minion('G', 5, Point(7, 3)))
    }

    "equals to example after 2 round" in {
      val s = round(state, 2)

      minion(s, 0) mustBe (Minion('G', 0, Point(3, 1)))
      minion(s, 1) mustBe (Minion('G', 1, Point(4, 2), 3, 197))
      minion(s, 2) mustBe (Minion('G', 2, Point(5, 1)))
      minion(s, 3) mustBe (Minion('G', 3, Point(2, 3)))
      minion(s, 4) mustBe (Minion('E', 4, Point(4, 3), 3, 197))
      minion(s, 5) mustBe (Minion('G', 5, Point(6, 3)))
    }
  }

  "round test small" must {
    "equals to example" in {
      val state = Dungeon.parse(
        s"""|#######
            |#.E...#
            |#.....#
            |#...G.#
            |#######"""
          .stripMargin
          .lines
      ).round()

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
         |#######"""
        .stripMargin
        .lines
    )

    "equals to example round 1" in {
      val state1 = round(state, 1)

      minion(state1, 0) mustBe (Minion('G', 0, Point(3, 1)))
      minion(state1, 1) mustBe (Minion('E', 1, Point(4, 2), 3, 197))
      minion(state1, 2) mustBe (Minion('G', 2, Point(5, 2), 3, 197))
      minion(state1, 3) mustBe (Minion('G', 3, Point(5, 3), 3, 197))
      minion(state1, 4) mustBe (Minion('G', 4, Point(3, 3)))
      minion(state1, 5) mustBe (Minion('E', 5, Point(5, 4), 3, 197))
    }

    "equals to example round 2" in {
      val state1 = round(state, 2)

      minion(state1, 0) mustBe (Minion('G', 0, Point(4, 1)))
      minion(state1, 1) mustBe (Minion('E', 1, Point(4, 2), 3, 188))
      minion(state1, 2) mustBe (Minion('G', 2, Point(5, 2), 3, 197))
      minion(state1, 3) mustBe (Minion('G', 3, Point(5, 3), 3, 197))
      minion(state1, 4) mustBe (Minion('G', 4, Point(3, 3)))
      minion(state1, 5) mustBe (Minion('E', 5, Point(5, 4), 3, 197))
    }

    "equals to example round 23" in {
      val s = round(state, 23)

      minion(s, 0) mustBe (Minion('G', 0, Point(3, 1)))
      minion(s, 1) mustBe (Minion('E', 1, Point(4, 2), 3, 197))
      minion(s, 2) mustBe (Minion('G', 2, Point(5, 2), 3, 197))
      minion(s, 3) mustBe (Minion('G', 3, Point(5, 3), 3, 197))
      minion(s, 4) mustBe (Minion('G', 4, Point(3, 3)))
      minion(s, 5) mustBe (Minion('E', 5, Point(5, 4), 3, 197))
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
              |#######"""
            .stripMargin
            .lines
        )

      score mustBe (36334)
    }
  }
}
