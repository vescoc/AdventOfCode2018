package aoc

import org.scalatest.{WordSpec, MustMatchers}

import Day17._

class Day17Spec extends WordSpec with MustMatchers {
  "parse" must {
    "parse test data" in {
      val ground = parse(
        s"""|x=495, y=2..7
            |y=7, x=495..501
            |x=501, y=3..7
            |x=498, y=2..4
            |x=506, y=1..2
            |x=498, y=10..13
            |x=504, y=10..13
            |y=13, x=498..504""".stripMargin.lines
      )

      println(ground)
    }
  }

  "ant" must {
    "walk" in {
      val ground = parse(
        s"""|x=495, y=2..7
            |y=7, x=495..501
            |x=501, y=3..7
            |x=498, y=2..4
            |x=506, y=1..2
            |x=498, y=10..13
            |x=504, y=10..13
            |y=13, x=498..504""".stripMargin.lines
      )

      val ant = ground.ant()

      (1 to 200) foreach { _ => ant.walk() }

      println(ant)
    }
  }
}
