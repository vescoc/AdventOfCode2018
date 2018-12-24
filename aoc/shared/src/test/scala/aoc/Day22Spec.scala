package aoc

import org.scalatest.{MustMatchers, WordSpec}

import Day22._

class Day22Spec extends WordSpec with MustMatchers {
  import MazeType._

  "test data" must {
    val maze = parse(
      """|depth: 510
         |target: 10,10""".stripMargin
    )

    "parse" in {
      maze.depth mustBe (510)
      maze.target mustBe ((10, 10))
    }

    "equals to test values" in {
      maze(0, 0).geologicalIndex mustBe (0)
      maze(0, 0).erosionLevel mustBe (510)
      maze(0, 0).mazeType mustBe (Rocky)

      maze(1, 0).geologicalIndex mustBe (16807)
      maze(1, 0).erosionLevel mustBe (17317)
      maze(1, 0).mazeType mustBe (Wet)

      maze(0, 1).geologicalIndex mustBe (48271)
      maze(0, 1).erosionLevel mustBe (8415)
      maze(0, 1).mazeType mustBe (Rocky)

      maze(1, 1).geologicalIndex mustBe (145722555)
      maze(1, 1).erosionLevel mustBe (1805)
      maze(1, 1).mazeType mustBe (Narrow)

      maze(10, 10).geologicalIndex mustBe (0)
      maze(10, 10).erosionLevel mustBe (510)
      maze(10, 10).mazeType mustBe (Rocky)
    }

    "risk level equals to test value" in {
      maze.riskLevel mustBe (114)
    }
  }
}
