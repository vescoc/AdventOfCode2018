package aoc

import org.scalatest.{MustMatchers, WordSpec}

import Day25._

class Day25Spec extends WordSpec with MustMatchers {
  "test data" must {
    "1 constellation" in {
      val constellations = Point.constellations(
        Point.parse(
          """|0,0,0,0
             |3,0,0,0
             |0,3,0,0
             |0,0,3,0
             |0,0,0,3
             |0,0,0,6
             |9,0,0,0
             |6,0,0,0
             |12,0,0,0""".stripMargin.lines
        )
      )

      constellations.size mustBe (1)
    }

    "2 constellations" in {
      val constellations = Point.constellations(
        Point.parse(
          """|0,0,0,0
             |3,0,0,0
             |0,3,0,0
             |0,0,3,0
             |0,0,0,3
             |0,0,0,6
             |9,0,0,0
             |12,0,0,0""".stripMargin.lines
        )
      )

      constellations.size mustBe (2)
    }

    "4 constellations" in {
      val constellations = Point.constellations(
        Point.parse(
          """|-1,2,2,0
             |0,0,2,-2
             |0,0,0,-2
             |-1,2,0,0
             |-2,-2,-2,2
             |3,0,2,-1
             |-1,3,2,2
             |-1,0,-1,0
             |0,2,1,-2
             |3,0,0,0""".stripMargin.lines
        )
      )

      //constellations foreach println

      constellations.size mustBe (4)
    }

    "3 constellations" in {
      val constellations = Point.constellations(
        Point.parse(
          """|1,-1,0,1
             |2,0,-1,0
             |3,2,-1,0
             |0,0,3,1
             |0,0,-1,-1
             |2,3,-2,0
             |-2,2,0,0
             |2,-2,0,-1
             |1,-1,0,-1
             |3,2,0,2""".stripMargin.lines
        )
      )

      //constellations foreach println

      constellations.size mustBe (3)
    }

    "8 constellations" in {
      val constellations = Point.constellations(
        Point.parse(
          """|1,-1,-1,-2
             |-2,-2,0,1
             |0,2,1,3
             |-2,3,-2,1
             |0,2,3,-2
             |-1,-1,1,-2
             |0,-2,-1,0
             |-2,2,3,-1
             |1,2,2,0
             |-1,-2,0,-2""".stripMargin.lines
        )
      )

      //constellations foreach println

      constellations.size mustBe (8)
    }
  }
}
