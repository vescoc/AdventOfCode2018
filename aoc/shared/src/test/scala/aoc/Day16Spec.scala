package aoc

import org.scalatest.{MustMatchers, WordSpec}

import Day16._

class Day16Spec extends WordSpec with MustMatchers {
  val data = parse(
    """|Before: [3, 2, 1, 1]
       |9 2 1 2
       |After:  [3, 2, 2, 1]""".stripMargin.lines
  )

  "parse" must {
    "parse sample data" in {
      val sample = data.samples(0)

      sample.before mustBe (Registries(3, 2, 1, 1))
      sample.istruction mustBe (Istruction(9, 2, 1, 2))
      sample.after mustBe (Registries(3, 2, 2, 1))
    }
  }

  "test test data" must {
    "returns" in {
      val sample = data.samples(0)

      sample.test().toSet mustBe (
        Set(
          OpCodes.mulr,
          OpCodes.addi,
          OpCodes.seti
        )
      )
    }
  }
}
