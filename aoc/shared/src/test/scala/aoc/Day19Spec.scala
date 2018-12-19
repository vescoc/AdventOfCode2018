package aoc

import org.scalatest.{WordSpec, MustMatchers}

import Day19._

class Day19Spec extends WordSpec with MustMatchers {
  "cpu" must {
    val (Some(cpu), istructions) = CPU.parse(
      """|#ip 0
         |seti 5 0 1
         |seti 6 0 2
         |addi 0 1 0
         |addr 1 2 3
         |setr 1 0 0
         |seti 8 0 4
         |seti 9 0 5""".stripMargin.lines
    )

    "parse sample data" in {
      istructions.size mustBe (7)
    }

    "run" in {
      val r = cpu.run(Registries(), istructions)

      r(0) mustBe (6)
    }
  }
}
