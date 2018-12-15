package aoc

import scala.annotation.tailrec
import scala.util.Try

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

  type Action = PartialFunction[(State, List[Minion]), (State, List[Minion])]

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
          .minBy { minion => state.dungeon.calcMinBy(minion.position) }
          .fightWith(current)

        if (enemy.isKilled)
          (state.killed(enemy), remainder.filterNot { minion => minion.id == enemy.id })
        else
          (
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

        if (enemies.isEmpty)
          Map.empty
        else {
          @tailrec
          def findPaths(paths: Set[Path], candidates: Set[Path], set: Set[Point]): Set[Path] = {
            if (candidates.isEmpty)
              paths
                .map { path => path.reverse }
            else {
              val set = paths.flatten ++ candidates.flatten

              val (newPaths, newCandidates, newSet) = candidates
                .foldLeft((paths, Set.empty[Path], set)) { (acc, path) =>
                  val (paths, candidates, set) = acc

                  val point = path.head

                  val (newPaths, newCandidates, newSet) = (
                    for {
                      c <- Cardinals
                      np = point + c
                      if set.contains(np) == false
                    } yield {
                      val npath = np :: path
                      if (candidates.contains(npath) || paths.contains(npath))
                        (None, None, None)
                      else
                        state(np) match {
                          case _: OpenCavern =>
                            (None, Some(npath), Some(np))
                          case _: Minion =>
                            (Some(npath), None, None)
                          case _ =>
                            (None, None, Some(np))
                        }
                    }
                  )
                    .foldLeft((paths, candidates, set)) { (acc, value) =>
                      (acc._1 ++ value._1, acc._2 ++ value._2, acc._3 ++ value._3)
                    }

                  val r = (newPaths, newCandidates, newSet)

                  r
                }

              findPaths(newPaths, newCandidates, newSet)
            }
          }

          val paths = findPaths(Set.empty, Set(List(current.position)), Set(current.position))

          enemies
            .flatMap { enemy =>
              Try {
                Some(
                  enemy -> paths
                    .filter { path =>
                      path.last == enemy.position
                    }
                    .groupBy { _.size }
                    .minBy { p =>
                      p._1
                    }
                    ._2
                    .minBy { path =>
                      state.dungeon.calcMinBy(path(1))
                    }
                )
              } getOrElse None
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

    def rest(state: State): Action = { case v => v }
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
      def turn(state: State, minion: Minion, remainder: List[Minion]): (State, List[Minion]) =
        (minion.attack(state) orElse minion.move(state) orElse minion.rest(state))((state, remainder))

      @tailrec
      def round(state: State, minions: List[Minion]): State =
        minions match {
          case minion :: remainder =>
            val (newState, newRemainder) = turn(state, minion, remainder)
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
        (count * (state.minions.map { _.hitpoints }.sum), state)
      else
        round(count + 1, state.round())

    round()
  }

  def main(args: Array[String]): Unit = {
    //val state = Dungeon.parse(Source.fromResource("input-15.data").getLines())
  }
}
