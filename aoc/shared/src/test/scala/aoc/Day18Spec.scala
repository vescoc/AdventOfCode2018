package aoc

import org.scalatest.{MustMatchers, WordSpec}

import Day18._

class Day18Spec extends WordSpec with MustMatchers {
  "landscape" must {
    val landscape = Landscape(
      """|.#.#...|#.
         |.....#|##|
         |.|..|...#.
         |..|#.....#
         |#.#|||#|#|
         |...#.||...
         |.|....|...
         |||...#|.#|
         ||.||||..|.
         |...#.|..|.""".stripMargin.lines.toList
    )

    "evolve 1" in {
      landscape.evolve() mustBe (
        Landscape(
          """|.......##.
             |......|###
             |.|..|...#.
             |..|#||...#
             |..##||.|#|
             |...#||||..
             |||...|||..
             ||||||.||.|
             |||||||||||
             |....||..|.""".stripMargin.lines.toList
        )
      )
    }

    "evolve after 10" in {
      landscape.evolve(10) mustBe (
        Landscape(
          """|.||##.....
             |||###.....
             |||##......
             ||##.....##
             ||##.....##
             ||##....##|
             |||##.####|
             |||#####|||
             |||||#|||||
             |||||||||||""".stripMargin.lines.toList
        )
      )
    }

    "resourceValue after 10 minutes" in {
      landscape.evolve(10).resourceValue mustBe (1147)
    }

    "resourceValue for 1000 minutes" in {
      landscape.evolve(1000).resourceValue mustBe (landscape.evolveRaw(1000).resourceValue)
    }

    "resourceValue for 1001 minutes" in {
      landscape.evolve(1001).resourceValue mustBe (landscape.evolveRaw(1001).resourceValue)
    }

    "resourceValue for 1028 minutes" in {
      landscape.evolve(1028).resourceValue mustBe (landscape.evolveRaw(1028).resourceValue)
    }

    "resourceValue for 1234 minutes" in {
      landscape.evolve(1234).resourceValue mustBe (landscape.evolveRaw(1234).resourceValue)
    }
  }

  "landscape big" must {
    val landscape = Landscape(Source.fromResource("input-18.data").getLines().toList)

    "resourceValue for 1234 minutes raw vs opt" in {

      landscape.evolve(1234).resourceValue mustBe (landscape.evolveRaw(1234).resourceValue)
    }
  }
}
