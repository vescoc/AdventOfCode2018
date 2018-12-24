package aoc

import org.scalatest.{WordSpec, MustMatchers}

import Day24._

class Day24Spec extends WordSpec with MustMatchers {
  "method parse" must {
    val testData = """|Immune System:
                      |17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
                      |989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3
                      |
                      |Infection:
                      |801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
                      |4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4""".stripMargin

    val state = State.parse(testData.lines)

    "parse test data" in {
      state.immuneSystem.filter { _.id == 1 }.head.units mustBe (17)

      state.infection.filter { _.id == 2 }.head.hitpoints mustBe (2961)
    }

    "round 1" in {
      val s = State.round(state)

      s.immuneSystem.size mustBe (1)
      s.immuneSystem(0).id mustBe (2)
      s.immuneSystem(0).units mustBe (905)
    }

    "game" in {
      val s = State.game(state)

      s.units mustBe (5216)
    }
  }
}
