package aoc

import scala.annotation.tailrec

object Day15 {
  val Cardinals = List(
    Point(-1, 0),
    Point(+1, 0),
    Point(0, -1),
    Point(0, +1)
  )

  object MinionType extends Enumeration {
    type MinionType = Char

    val Elf = 'E'
    val Goblin = 'G'
  }

  import MinionType._

  case class Point(x: Int, y: Int) {
    def +(that: Point) = Point(x + that.x, y + that.y)
  }

  type Action = PartialFunction[(State, List[Minion]), (Minion, State, List[Minion])]

  type Path = List[Point]

  sealed trait DungeonElement {
    val position: Point
  }

  case class Wall(position: Point) extends DungeonElement
  case class OpenCavern(position: Point) extends DungeonElement

  case class Minion(minionType: MinionType, id: Int, position: Point, force: Int = 3, hitpoints: Int = 200) extends DungeonElement { current =>
    def isKilled = hitpoints <= 0

    def fightWith(that: Minion) = copy(hitpoints = hitpoints - that.force)

    def attack(state: State): Action = new Action {
      lazy val neighborEnemies: List[Minion] =
        (
          for {
            c <- Cardinals
          } yield {
            state(position + c) match {
              case m @ Minion(minionType, _, p, _, _) if current.minionType != minionType =>
                Some(m)
              case _ =>
                None
            }
          }
        ).flatten

      def isDefinedAt(v: (State, List[Minion])) = neighborEnemies.isEmpty == false

      def apply(v: (State, List[Minion])) = {
        val (state, remainder) = v

        val enemy = neighborEnemies
          .groupBy { _.hitpoints }
          .minBy { _._1 }
          ._2
          .minBy { minion => state.dungeon.calcMinBy(minion.position) }
          .fightWith(current)

        if (enemy.isKilled)
          (current, state.killed(enemy), remainder.filterNot { minion => minion.id == enemy.id })
        else
          (
            current,
            state.adjourn(enemy),
            remainder.map { minion =>
              if (minion.id == enemy.id)
                enemy
              else
                minion
            }
          )
      }
    }

    def move(state: State): Action = new Action {
      lazy val pathToEnemies: Map[Minion, Path] = {
        val enemies = state
          .minions
          .map { minion =>
            if (minion.minionType != current.minionType)
              Some(minion)
            else
              None
          }
          .flatten

        lazy val enemyNear = (
          for {
            c <- Cardinals
            np = current.position + c
          } yield {
            state(np) match {
              case m @ Minion(minionType, _, _, _, _) if (current.minionType != minionType) =>
                Some(m)
              case _ =>
                None
            }
          }
        )
          .flatten
          .isEmpty == false

        if (enemies.isEmpty || enemyNear)
          Map.empty
        else {
          @tailrec
          def findPaths(map: Map[Point, Path], remainder: Set[Point]): Map[Point, Path] = {
            remainder.toList match {
              case head :: tail =>
                if (map.contains(head)) {
                  findPaths(
                    map,
                    tail.toSet ++ (
                      for {
                        c <- Cardinals
                        np = head + c
                        if map.contains(np) == false && state(np).isInstanceOf[OpenCavern]
                      } yield { np }
                    )
                  )
                } else {
                  val paths = for {
                    c <- Cardinals
                    np = head + c
                    if map.contains(np)
                  } yield { head :: map(np) }

                  if (paths.isEmpty) {
                    findPaths(map, tail.toSet)
                  } else {
                    val betterPath = paths
                      .groupBy { _.size }
                      .minBy { _._1 }
                      ._2
                      .minBy { path =>
                        if (path.size > 1)
                          state.dungeon.calcMinBy(path(path.size - 2))
                        else
                          0
                      }

                    findPaths(
                      map + (head -> betterPath),
                      tail.toSet + head
                    )
                  }
                }
              case _ =>
                map
                  .map { p =>
                    p._1 -> p._2.reverse
                  }
            }
          }

          val paths = findPaths(Map(current.position -> List(current.position)), Set(current.position))

          enemies
            .flatMap { enemy =>
              val ps = for {
                c <- Cardinals
                np = enemy.position + c
                if paths.contains(np)
              } yield { paths(np) }

              if (ps.isEmpty)
                None
              else {
                Some(
                  enemy ->
                    (ps
                      .groupBy { _.size }
                      .minBy { _._1 }
                      ._2
                      .minBy { path => state.dungeon.calcMinBy(path(1)) }
                    )
                )
              }
            }
            .toMap
        }          
      }

      def apply(v: (State, List[Minion])) = {
        val (state, remainder) = v

        val (minion, path) = pathToEnemies
          .groupBy { _._2.size }
          .minBy { _._1 }
          ._2
          .minBy { p => state.dungeon.calcMinBy(p._2(1)) }

        val movedMinion = copy(position = path(1))

        (
          movedMinion,
          state.adjourn(movedMinion),
          remainder
            .map { minion =>
              if (minion.id == movedMinion.id)
                movedMinion
              else
                minion
            }
        )
      }

      def isDefinedAt(v: (State, List[Minion])) = pathToEnemies.isEmpty == false
    }

    def rest(state: State): Action = { case v => (this, v._1, v._2) }
  }

  case class Dungeon(data: Array[String]) {
    override def toString() = data.mkString("\n")

    lazy val Width = data(0).size
    lazy val Height = data.size

    def apply(position: Point) = data(position.y)(position.x)

    def calcMinBy(position: Point) = position.y * Width + position.x
  }

  case class State(dungeon: Dungeon, minions: List[Minion], minionsDead: List[Minion]) {
    def round(): State = {
      def turn(minion: Minion, state: State, remainder: List[Minion]): (State, List[Minion]) = {
        val move = minion.move(state)
        if (move.isDefinedAt((state, remainder))) {
          val (newMinion, newState, newRemainder) = move.apply((state, remainder))

          val attack = newMinion.attack(newState)
          if (attack.isDefinedAt((newState, newRemainder))) {
            val r = attack.apply((newState, newRemainder))
            (r._2, r._3)
          } else {
            (newState, newRemainder)
          }
        } else {
          val attack = minion.attack(state)
          if (attack.isDefinedAt((state, remainder))) {
            val r = attack.apply((state, remainder))
            (r._2, r._3)
          } else {
            (state, remainder)
          }
        }
      }

      @tailrec
      def round(state: State, minions: List[Minion]): State =
        minions match {
          case minion :: remainder =>
            val (newState, newRemainder) = turn(minion, state, remainder)
            round(newState, newRemainder)
          case _ =>
            state
        }

      round(this, minions.sortBy { minion => dungeon.calcMinBy(minion.position) })
    }

    def killed(that: Minion): State = copy(
      minions = minions.filterNot { minion => minion.id == that.id },
      minionsDead = that :: minionsDead
    )

    def adjourn(that: Minion): State = copy(
      minions = minions
        .map { minion =>
          if (that.id == minion.id)
            that
          else
            minion
        }
    )

    def apply(position: Point): DungeonElement =
      dungeon(position) match {
        case '#' =>
          Wall(position)
        case '.' =>
          minions.find { _.position == position } match {
            case Some(minion) =>
              minion
            case None =>
              OpenCavern(position)
          }
      }
  }

  object Dungeon {
    def parse(lines: Iterator[String]): State = {
      val ids = Iterator.from(0)

      val (mapLines, minions) = lines
        .zipWithIndex
        .map { p =>
          val (l, y) = p

          l
            .zipWithIndex
            .map { p =>
              val (c, x) = p

              c match {
                case '#' | '.' =>
                  (c, None)
                case 'G' | 'E' =>
                  ('.', Some(Minion(c, ids.next, Point(x, y))))
              }
            }
            .foldLeft(("", List.empty[Option[Minion]])) { (acc, v) =>
              (
                acc._1 + v._1.toString,
                v._2 :: acc._2,
              )
            }
        }
        .foldLeft((List.empty[String], List.empty[Option[Minion]])) { (acc, v) =>
          (
            acc._1 ++ List(v._1),
            acc._2 ++ v._2,
          )
        }

      State(Dungeon(mapLines.toArray), minions.flatten, List.empty[Minion])
    }
  }

  def solution1(lines: Iterator[String]): (Int, State) = {
    val state = Dungeon.parse(lines)

    @tailrec
    def round(count: Int = 0, state: State = state): (Int, State) =
      if (state.minions.groupBy { _.minionType }.size == 1)
        ((count - 1) * (state.minions.map { _.hitpoints }.sum), state)
      else
        round(count + 1, state.round())

    round()
  }

  def main(args: Array[String]): Unit = {
    val lines = Source
      .fromResource("input-15.data")
      .getLines()

    println(s"solution 1: ${solution1(lines)}")
  }
}
