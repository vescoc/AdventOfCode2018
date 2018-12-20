package aoc

import org.scalatest.{WordSpec, MustMatchers}

import aoc.Day20._

class Day20Spec extends WordSpec with MustMatchers {
  "furthest path" must {
    def test(path: String, value: Int) = s"for $path equals to $value" in {
      furthest(path) mustBe (value)
    }

    test("^WNE$", 3)
    test("^ENWWW(NEEE|SSE(EE|N))$", 10)
    test("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$", 18)
    test("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$", 23)
    test("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$", 31)
  }
}
