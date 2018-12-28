package aoc

import org.scalatest.{MustMatchers, WordSpec}

import aoc.Day20._

class Day20Spec extends WordSpec with MustMatchers {
  def n(x: Int, y: Int) = (x, y)

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

  "graph" must {
    "build empty" in {
      val graph = buildGraph("^$")

      graph.nodes mustBe (Set((0, 0)))
      graph.edges mustBe (Set.empty)
    }

    "build WNE" in {
      val graph = buildGraph("^WNE$")

      graph.nodes mustBe (Set(n(0, 0), n(-1, 0), n(-1, 1), n(0, 1)))
      graph.edges mustBe (
        Set(
          n(0, 0) -> n(-1, 0),
          n(-1, 0) -> n(-1, 1),
          n(-1, 1) -> n(0, 1)
        )
      )

      val (costs, _) = graph.dijkstra((0, 0))

      costs.maxBy { _._2 }._2 mustBe (3)
    }

    "build ENWWW(NEEE|SSE(EE|N))" in {
      val graph = buildGraph("^ENWWW(NEEE|SSE(EE|N))$")

      val (costs, _) = graph.dijkstra((0, 0))

      costs.maxBy { _._2 }._2 mustBe (10)
    }

    "build ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN" in {
      val graph = buildGraph("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")

      val (costs, _) = graph.dijkstra((0, 0))

      costs.maxBy { _._2 }._2 mustBe (18)
    }

    "build WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))" in {
      val graph = buildGraph("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")

      val (costs, _) = graph.dijkstra((0, 0))

      costs.maxBy { _._2 }._2 mustBe (31)
    }
  }
}
