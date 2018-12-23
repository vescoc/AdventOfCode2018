package aoc

import org.scalatest.{WordSpec, MustMatchers}

import Day23._

class Day23Spec extends WordSpec with MustMatchers {
  "parse with test data" must {
    val nanobots = Nanobots.parse(
      """|pos=<0,0,0>, r=4
         |pos=<1,0,0>, r=1
         |pos=<4,0,0>, r=3
         |pos=<0,2,0>, r=1
         |pos=<0,5,0>, r=3
         |pos=<0,0,3>, r=1
         |pos=<1,1,1>, r=1
         |pos=<1,1,2>, r=1
         |pos=<1,3,1>, r=1""".stripMargin.lines
    )

    "find the strongest nanobot" in {
      nanobots.strongest mustBe (Nanobot(Point(0, 0, 0), 4))
    }

    "count in range of strongest" in {
      nanobots.inRange(nanobots.strongest).size mustBe (7)
    }
  }
}
