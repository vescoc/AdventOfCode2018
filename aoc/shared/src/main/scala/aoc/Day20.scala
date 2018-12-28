package aoc

import scala.annotation.tailrec

object Day20 {
  type Node = (Int, Int)
  type Edge = (Node, Node)

  def buildGraph(path: String): Graph[Node, Edge] = {
    val i = path.toIterator

    implicit class ExtNode(that: Node) {
      def +(other: Node) = (that._1 + other._1, that._2 + other._2)
    }

    val directions = Map(
      'N' -> ((0, +1)),
      'S' -> ((0, -1)),
      'E' -> ((+1, 0)),
      'W' -> ((-1, 0))
    )

    @tailrec
    def build(
      nodes: Set[Node] = Set.empty,
      edges: Set[Edge] = Set.empty,
      currentNodes: Set[Node] = Set.empty,
      currentBranches: List[Node] = List.empty,
      stack: List[(Set[Node], List[Node])] = List.empty
    ): Graph[Node, Edge] = {
      val c = i.next
      c match {
        case '^' if stack.isEmpty =>
          val newCurrentNodes = Set((0, 0))
          build(nodes ++ newCurrentNodes, edges, newCurrentNodes, currentBranches, stack)
        case '$' if stack.isEmpty && currentBranches.isEmpty =>
          Graph(nodes, edges)
        case 'N' | 'W' | 'S' | 'E' =>
          val newEdges = currentNodes.map { node =>
            node -> (node + directions(c))
          }
          val newNodes = newEdges.map { edge =>
            edge._2
          }
          build(
            nodes ++ newNodes,
            edges ++ newEdges,
            newNodes,
            currentBranches,
            stack
          )
        case '(' =>
          build(nodes, edges, currentNodes, List.empty, (currentNodes, currentBranches) :: stack)
        case '|' =>
          val (ns, _) = stack.head

          build(nodes, edges, ns, currentBranches ++ currentNodes, stack)
        case ')' =>
          val (_, branches) = stack.head
          build(nodes, edges, currentBranches.toSet, branches, stack.tail)
      }
    }

    build()
  }

  def furthest(path: String): Int = {
    val i = path.toIterator

    @tailrec
    def count(current: Int, list: List[Int], stack: List[List[Int]]): Int =
      i.next match {
        case '^' if stack.isEmpty && list.isEmpty =>
          count(0, list, stack)
        case '$' if stack.isEmpty && list.isEmpty =>
          current
        case 'N' | 'S' | 'E' | 'W' =>
          count(current + 1, list, stack)
        case '(' =>
          count(0, List.empty, (current :: list) :: stack)
        case ')' =>
          stack match {
            case head :: tail =>
              head match {
                case last :: newList =>
                  count(
                    last + {
                      val l = current :: list
                      if (l.contains(0))
                        0
                      else
                        l.max
                    },
                    newList,
                    tail
                  )
                case _ =>
                  throw new MatchError(head)
              }
            case _ =>
              throw new MatchError(stack)
          }
        case '|' =>
          count(0, current :: list, stack)
      }

    count(0, List.empty, List.empty)
  }

  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("input-20.data")
      .getLines()
      .mkString

    val graph = buildGraph(input)
    val (costs, _) = graph.dijkstra((0, 0))

    println(s"solution 1: ${costs.maxBy { _._2 }._2}")
    println(s"solution 2: ${costs.filter { _._2 >= 1000 }.size}")
  }
}
