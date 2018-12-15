package aoc

import org.scalatest.{MustMatchers, WordSpec}

import aoc.Day15._

class Day15Spec extends WordSpec with MustMatchers {
  def minion(state: State, id: Int) =
    state.minions.filter { _.id == id }.head

  "round test big" must {
    "equals to example" in {
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

      val newState = state.round()

      minion(newState, 0) mustBe (Minion('G', 0, Point(2, 1)))
      minion(newState, 1) mustBe (Minion('G', 1, Point(4, 2)))
      minion(newState, 2) mustBe (Minion('G', 2, Point(6, 1)))
      minion(newState, 3) mustBe (Minion('G', 3, Point(2, 4)))
      minion(newState, 4) mustBe (Minion('E', 4, Point(4, 3)))
      minion(newState, 5) mustBe (Minion('G', 5, Point(8, 3)))
    }
  }

  "round test small" must {
    "equals to example" ignore {
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

  "solution 1 test" must {
    "equals 36334" ignore {
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
