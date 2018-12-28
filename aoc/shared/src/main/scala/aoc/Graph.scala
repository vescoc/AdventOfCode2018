package aoc

import scala.annotation.tailrec

case class Graph[N, E <: (N, N)](nodes: Set[N], edges: Set[E]) {
  def dijkstra(startNode: N, p: (N, N) => Int = Graph.unit): (Map[N, Int], Map[N, N]) = {
    val neighbours = edges
      .foldLeft(nodes.map { node =>
        node -> Set.empty[N]
      }.toMap) { (acc, edge) =>
        acc + (
          edge._1 -> (
            acc(edge._1) + edge._2
          )
        )
      }
      .toMap

    val initialNeighbours = neighbours(startNode)

    @tailrec
    def step(
      s: Set[N] = Set(startNode),
      t: Set[N] = nodes - startNode,
      f: Map[N, Int] = Map(startNode -> 0) ++ initialNeighbours.map { node =>
        (node -> p(startNode, node))
      },
      j: Map[N, N] = Map.empty ++ initialNeighbours.map { node =>
        (node -> startNode)
      },
    ): (Map[N, Int], Map[N, N]) =
      if (t.isEmpty)
        (f, j)
      else {
        val next = f.filter { p =>
          t.contains(p._1)
        }
        if (next.isEmpty)
          (f, j)
        else {
          val (node, cost) = next.minBy { _._2 }
          val nextT = t - node
          val nextS = s + node

          @tailrec
          def nextFJ(
            nodes: List[N] = neighbours(node).toList,
            f: Map[N, Int] = f,
            j: Map[N, N] = j
          ): (Map[N, Int], Map[N, N]) =
            nodes match {
              case n :: tail =>
                val nf = cost + p(node, n)
                f.get(n) match {
                  case Some(of) if (of > nf) =>
                    nextFJ(tail, f + (n -> nf), j + (n -> node))
                  case None =>
                    nextFJ(tail, f + (n -> nf), j + (n -> node))
                  case _ =>
                    nextFJ(tail, f, j)
                }
              case _ =>
                (f, j)
            }

          val (nextF, nextJ) = nextFJ()

          step(nextS, nextT, nextF, nextJ)
        }
      }

    step()
  }
}
object Graph {
  val unit = (n1: Any, n2: Any) => 1
}
