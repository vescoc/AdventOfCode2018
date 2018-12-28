package aoc

import scala.annotation.tailrec

object Day22 {
  val SwitchToolCost = 7
  val ChangePositionCost = 1

  val Cardinals = List(
    (0, -1),
    (0, +1),
    (-1, 0),
    (+1, 0)
  )

  val depthRe = """depth: (\d+)""".r
  val targetRe = """target: (\d+),(\d+)""".r

  object MazeType extends Enumeration {
    type MazeType = Int

    val Rocky = 0
    val Wet = 1
    val Narrow = 2
  }
  import MazeType._

  object ToolType extends Enumeration {
    type ToolType = Value

    val ClimbingGear, Torch, Neither = Value
  }
  import ToolType._

  implicit class TypeTypeExt(toolType: ToolType) {
    def isUsableIn(mazeType: MazeType) = (
      mazeType == Rocky && toolType != Neither ||
      mazeType == Wet && toolType != Torch ||
      mazeType == Narrow && toolType != ClimbingGear
    )
  }

  implicit class Tuple2Ext(that: (Int, Int)) {
    def +(other: (Int, Int)) = (that._1 + other._1, that._2 + other._2)
  }

  case class Maze(depth: Int, target: (Int, Int), width: Int, height: Int) {
    private lazy val map = {
      val arr = Array.ofDim[(Long, Long, MazeType)](width, height)

      for {
        y <- 0 until height
        x <- 0 until width
      } {
        val geologicalIndex = (x, y) match {
          case (0, 0) | `target` => 0
          case (0, _)            => y * 48271L
          case (_, 0)            => x * 16807L
          case (_, _)            => arr(x - 1)(y)._2 * arr(x)(y - 1)._2
        }

        val erosionLevel = (geologicalIndex + depth) % 20183
        val mazeType = (erosionLevel % 3).toInt

        arr(x)(y) = (geologicalIndex, erosionLevel, mazeType)
      }

      arr
    }

    trait Info {
      def geologicalIndex: Long
      def erosionLevel: Long
      def mazeType: Int
    }

    def apply(x: Int, y: Int): Info = {
      val v = map(x)(y)

      new Info {
        val geologicalIndex = v._1
        val erosionLevel = v._2
        val mazeType = v._3
      }
    }

    lazy val riskLevel =
      (
        for {
          x <- 0 to target._1
          y <- 0 to target._2
        } yield { map(x)(y)._3 }
      ).sum

    type Node = ((Int, Int), ToolType)
    type Cost = Int

    def search(): (Int, List[Node]) = {
      def neighbors(node: Node): Set[(Node, Cost)] = {
        val (position, toolType) = node

        val mazeType = map(position._1)(position._2)._3

        val switchTool = (
          for {
            tool <- ToolType.values
            if tool != toolType && tool.isUsableIn(mazeType)
          } yield ((position, tool) -> SwitchToolCost)
        )

        val changePosition = (
          for {
            delta <- Cardinals
            newPosition = position + delta
            if newPosition._1 >= 0 && newPosition._2 >= 0 && newPosition._1 < width && newPosition._2 < height
            newMazeType = map(newPosition._1)(newPosition._2)._3
            if toolType.isUsableIn(newMazeType)
          } yield ((newPosition, toolType), ChangePositionCost)
        )

        switchTool ++ changePosition
      }

      val startNode = ((0, 0), Torch)
      val goal = ((target._1, target._2), Torch)

      def heuristicCost(node: Node) =
        Math.abs(goal._1._1 - node._1._1) + Math.abs(goal._1._2 - node._1._2) + (if (goal._2 != node._2) SwitchToolCost
                                                                                 else 0)

      @tailrec
      def step(
        closedSet: Set[Node] = Set.empty,
        openSet: Set[Node] = Set(startNode),
        cameFrom: Map[Node, Node] = Map.empty,
        gScore: Map[Node, Cost] = Map(startNode -> 0),
        fScore: Map[Node, Cost] = Map(startNode -> heuristicCost(startNode))
      ): (Int, List[Node]) = {
        @tailrec
        def pathToStart(currentNode: Node = goal, path: List[Node] = List.empty): List[Node] =
          if (currentNode == startNode)
            path
          else
            pathToStart(cameFrom(currentNode), currentNode :: path)

        if (openSet.isEmpty)
          sys.error("openSet empty")
        else {
          val (current, cost) = openSet
            .map { node =>
              node -> fScore.get(node)
            }
            .collect { case (node, Some(value)) => (node -> value) }
            .minBy { _._2 }

          if (current == goal)
            (cost, pathToStart())
          else {
            @tailrec
            def calcN(
              ns: List[(Node, Cost)] = neighbors(current).toList,
              openSet: Set[Node] = openSet - current,
              cameFrom: Map[Node, Node] = cameFrom,
              gScore: Map[Node, Cost] = gScore,
              fScore: Map[Node, Cost] = gScore
            ): (Set[Node], Map[Node, Node], Map[Node, Cost], Map[Node, Cost]) =
              ns match {
                case head :: tail =>
                  val (neighbor, neighborCost) = head
                  if (closedSet.contains(neighbor))
                    calcN(tail, openSet, cameFrom, gScore, fScore)
                  else {
                    val score = gScore(current) + neighborCost
                    if (openSet.contains(neighbor) && score >= gScore(neighbor))
                      calcN(tail, openSet, cameFrom, gScore, fScore)
                    else
                      calcN(
                        tail,
                        openSet + neighbor,
                        cameFrom + ((neighbor -> current)),
                        gScore + ((neighbor -> score)),
                        fScore + ((neighbor -> (score + heuristicCost(neighbor))))
                      )
                  }
                case _ =>
                  (openSet, cameFrom, gScore, fScore)
              }

            val (newOpenSet, newCameFrom, newGScore, newFScore) = calcN()

            step(closedSet + current, newOpenSet, newCameFrom, newGScore, newFScore)
          }
        }
      }

      step()
    }
  }

  def parse(data: String) = {
    val (Some(depth), Some(target)) = data.lines
      .foldLeft[(Option[Int], Option[(Int, Int)])]((None, None)) { (acc, line) =>
        (acc, line) match {
          case ((None, _), depthRe(depth)) =>
            (Some(depth.toInt), acc._2)
          case ((_, None), targetRe(x, y)) =>
            (acc._1, Some((x.toInt, y.toInt)))
        }
      }

    Maze(depth, target, (target._1 + 60) * 1, (target._2 + 60) * 1)
  }

  def main(args: Array[String]): Unit = {
    val maze = parse(Source.fromResource("input-22.data").mkString)

    println(s"solution 1: ${maze.riskLevel}")

    val solution2 = maze.search()

    println(s"solution 2: ${solution2._1}")
  }
}
