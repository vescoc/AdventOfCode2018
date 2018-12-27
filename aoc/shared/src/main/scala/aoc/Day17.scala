package aoc

object Day17 {
  val xre = """x=(\d+), y=(\d+)..(\d+)""".r
  val yre = """y=(\d+), x=(\d+)..(\d+)""".r

  object GroundElement extends Enumeration {
    type GroundElement = String

    val PrimarySource = "+"
    val Clay = "#"
    val Sand = "."
    val Water = "|"
    val RestWater = "~"
    val AntMark = "@"
  }

  import GroundElement._

  case class Ground(ground: Map[(Int, Int), GroundElement]) { g =>
    def apply(x: Int, y: Int) =
      ground.getOrElse(
        (x, y),
        if (x == 500 && y == 0) PrimarySource else Sand
      )

    private implicit class EPoint(point: (Int, Int)) {
      def min(that: (Int, Int)) = (Math.min(point._1, that._1), Math.min(point._2, that._2))
      def max(that: (Int, Int)) = (Math.max(point._1, that._1), Math.max(point._2, that._2))

      def +(that: (Int, Int)) = (point._1 + that._1, point._2 + that._2)
    }

    lazy val (min, max) = ground
      .foldLeft(((Int.MaxValue, Int.MaxValue), (Int.MinValue, Int.MinValue))) { (acc, value) =>
        (acc._1 min value._1, acc._2 max value._1)
      }

    override def toString = {
      val (visited, rest, ants) =
        Ant.ants.foldLeft((Set.empty[(Int, Int)], Set.empty[(Int, Int)], Set.empty[(Int, Int)])) { (acc, p) =>
          ((acc._1 ++ p._2.visitedPositions), (acc._2 ++ p._2.restWater), (acc._3 + p._2.currentPosition))
        }

      (
        for {
          y <- (min._2 - 1) to (max._2 + 1)
        } yield {
          (
            for {
              x <- (min._1 - 1) to (max._1 + 1)
            } yield {
              if (ants.contains((x, y)))
                AntMark
              else if (rest.contains((x, y)))
                RestWater
              else if (visited.contains((x, y)))
                Water
              else
                apply(x, y)
            }
          ).mkString("")
        }
      ).mkString("\n")
    }

    trait Walk {
      def walk(): Unit
      def done(): Boolean
      def water(): Int
      def restWater(): Int
    }

    def ant(): Walk = new Walk {
      val ant = Ant.VerticalAnt(None, (500, 0))

      def walk(): Unit = {
        Map(Ant.ants.toList: _*).map { _._2.run() }

        while (!Ant.disposeableAnts.isEmpty) Ant.ants -= Ant.disposeableAnts.remove(0)

        ()
      }

      def done() = ant.stateValue == Ant.StateValue.Done || ant.stateValue == Ant.StateValue.Filled

      def water() =
        ant.visitedPositions.filter { p =>
          p._2 >= g.min._2 && p._2 <= g.max._2
        }.size

      def restWater() =
        ant.restWater.size
    }

    object Ant {
      import scala.collection.mutable

      val nextId = Iterator.from(0)

      val disposeableAnts = mutable.ListBuffer.empty[Int]
      val ants = mutable.Map.empty[Int, State[_]]

      object Direction extends Enumeration {
        type Direction = (Int, Int)

        val Up = (0, -1)
        val Down = (0, 1)
        val Left = (-1, 0)
        val Right = (+1, 0)
      }

      object StateValue extends Enumeration {
        type StateValue = Value

        val Running, Wait, Done, Blocked, Filled = Value
      }
      import StateValue._

      trait State[S <: State[_]] {
        val id: Int
        val fatherId: Option[Int]
        val startPosition: (Int, Int)
        val visitedPositions: mutable.Set[(Int, Int)]
        val restWater: mutable.Set[(Int, Int)]

        var stateValue: StateValue
        var currentPosition: (Int, Int)

        var refCount: Int

        def visitable(position: (Int, Int)): Boolean =
          if (visitedPositions.contains(position))
            false
          else
            getFatherState(fatherId)
              .map { _.visitable(position) }
              .getOrElse(g(position._1, position._2) == Sand)

        def getFatherState(id: Option[Int]): Option[State[_]] =
          id match {
            case Some(v) =>
              ants.get(v)
            case None =>
              None
          }

        def run(): Unit

        def dispose() = {
          refCount -= 1
          if (refCount <= 0)
            disposeableAnts += id
        }
      }

      trait GoVerticalState extends State[GoVerticalState] with TwoChildState[GoVerticalState] {
        var dy: Int
      }

      trait GoHorizontalState extends State[GoHorizontalState] with SingleChildState[GoHorizontalState] {
        val dx: Int
      }

      trait SingleChildState[T <: State[T]] { self: State[T] =>
        def spawnAnt(): Unit = {
          childId = Some(VerticalAnt(Some(self.id), self.currentPosition).id)
          self.stateValue = Wait
        }

        var childId: Option[Int]
      }

      trait TwoChildState[T <: State[T]] { self: State[T] =>
        def spawnAnts(): Unit = {
          leftChildId = Some(HorizontalAnt(Some(self.id), self.currentPosition, -1).id)
          rightChildId = Some(HorizontalAnt(Some(self.id), self.currentPosition, +1).id)

          self.stateValue = Wait
        }

        var leftChildId: Option[Int]
        var rightChildId: Option[Int]
      }

      def VerticalAnt(fId: Option[Int], cp: (Int, Int)): GoVerticalState = {
        val l = ants.collect {
          case (_, ant: GoVerticalState) if ant.startPosition == cp => ant
        }

        if (l.isEmpty) {
          val ant = new GoVerticalState {
            val id = nextId.next
            val startPosition = cp
            val fatherId = fId
            val visitedPositions = mutable.Set.empty[(Int, Int)]
            val restWater = mutable.Set.empty[(Int, Int)]

            var refCount = 1
            var dy = +1
            var currentPosition = cp
            var stateValue = Running
            var leftChildId: Option[Int] = None
            var rightChildId: Option[Int] = None

            def run() = ants += (id -> (goDown orElse antsWait orElse goUp orElse done orElse filled)(this))
          }

          ants += (ant.id -> ant)

          ant
        } else {
          val ant = l.head
          ant.refCount += 1
          ant
        }
      }

      def HorizontalAnt(fId: Option[Int], cp: (Int, Int), d: Int): GoHorizontalState = {
        val l = ants.collect {
          case (_, ant: GoHorizontalState) if ant.startPosition == cp && ant.dx == d => ant
        }

        if (l.isEmpty) {
          val ant = new GoHorizontalState {
            val id = nextId.next
            val startPosition = cp
            val fatherId = fId
            val visitedPositions = mutable.Set.empty[(Int, Int)]
            val restWater = mutable.Set.empty[(Int, Int)]
            val dx = d

            var refCount = 1
            var currentPosition = cp
            var stateValue = Running
            var childId: Option[Int] = None

            def run() = ants += (id -> (goHorizontal orElse antWait orElse done orElse blocked)(this))
          }

          ants += (ant.id -> ant)

          ant
        } else {
          val ant = l.head
          ant.refCount += 1
          ant
        }
      }

      def done[T <: State[T]]: PartialFunction[T, T] = {
        case state if state.stateValue == Done =>
          state
      }

      def blocked[T <: State[T]]: PartialFunction[T, T] = {
        case state if state.stateValue == Blocked =>
          state
      }

      def filled[T <: State[T]]: PartialFunction[T, T] = {
        case state if state.stateValue == Filled =>
          state
      }

      def antWait: PartialFunction[GoHorizontalState, GoHorizontalState] = {
        case state if state.stateValue == Wait && state.childId != None =>
          val id = state.childId.get

          val cState = ants(id)

          if (cState.stateValue == Filled) {
            state.visitedPositions ++= cState.visitedPositions
            state.restWater ++= cState.restWater

            cState.dispose()

            state.childId = None
            state.stateValue = Running
          } else if (cState.stateValue == Done) {
            state.visitedPositions ++= cState.visitedPositions
            state.restWater ++= cState.restWater

            cState.dispose()

            state.childId = None
            state.stateValue = Done
          }

          state
      }

      def antsWait: PartialFunction[GoVerticalState, GoVerticalState] = {
        case state if state.stateValue == Wait && state.leftChildId != None && state.rightChildId != None =>
          val lid = state.leftChildId.get
          val rid = state.rightChildId.get

          val lState = ants(lid)
          val rState = ants(rid)

          if (lState.stateValue == Blocked && rState.stateValue == Blocked) {
            val restWater = (lState.visitedPositions ++ rState.visitedPositions) + state.currentPosition

            state.visitedPositions ++= restWater
            state.restWater ++= restWater

            lState.dispose()
            rState.dispose()

            state.leftChildId = None
            state.rightChildId = None

            state.dy = -1
            state.stateValue = Running
          } else if (lState.stateValue == Blocked && rState.stateValue == Done ||
                     lState.stateValue == Done && rState.stateValue == Blocked) {
            state.visitedPositions ++= (lState.visitedPositions ++ rState.visitedPositions)
            state.restWater ++= (lState.restWater ++ rState.restWater)

            lState.dispose()
            rState.dispose()

            state.leftChildId = None
            state.rightChildId = None

            state.stateValue = Done
          } else if (lState.stateValue == Done && rState.stateValue == Done) {
            state.visitedPositions ++= (lState.visitedPositions ++ rState.visitedPositions)
            state.restWater ++= (lState.restWater ++ rState.restWater)

            lState.dispose()
            rState.dispose()

            state.leftChildId = None
            state.rightChildId = None

            state.stateValue = Done
          }

          state
      }

      lazy val goHorizontal: PartialFunction[GoHorizontalState, GoHorizontalState] = {
        case state if state.stateValue == Running =>
          val down = state.currentPosition + Direction.Down
          if (state.visitable(down))
            state.spawnAnt()
          else {
            val newCurrentPosition = state.currentPosition + ((state.dx, 0))
            if (state.visitable(newCurrentPosition)) {
              state.currentPosition = newCurrentPosition
              state.visitedPositions += newCurrentPosition
            } else {
              state.stateValue = Blocked
            }
          }

          state
      }

      lazy val goDown: PartialFunction[GoVerticalState, GoVerticalState] = {
        case state if state.stateValue == Running && state.dy == +1 =>
          val down = state.currentPosition + Direction.Down
          if (down._2 > g.max._2)
            state.stateValue = Done
          else if (state.visitable(down)) {
            state.currentPosition = down
            state.visitedPositions += down
          } else
            state.spawnAnts()

          state
      }

      lazy val goUp: PartialFunction[GoVerticalState, GoVerticalState] = {
        case state if state.stateValue == Running && state.dy == -1 =>
          val up = state.currentPosition + Direction.Up
          if (state.currentPosition == state.startPosition)
            state.stateValue = Filled
          else {
            state.currentPosition = up
            state.dy = +1
            state.spawnAnts()
          }

          state
      }
    }
  }

  def parse(lines: Iterator[String]): Ground =
    Ground(
      lines
        .foldLeft(Map.empty[(Int, Int), GroundElement]) { (acc, line) =>
          acc ++ (
            line match {
              case xre(x, sy, ey) =>
                val (ix, isy, iey) = (x.toInt, sy.toInt, ey.toInt)
                for {
                  y <- isy to iey
                } yield { (ix, y) -> Clay }
              case yre(y, sx, ex) =>
                val (iy, isx, iex) = (y.toInt, sx.toInt, ex.toInt)
                for {
                  x <- isx to iex
                } yield { (x, iy) -> Clay }
            }
          )
        }
    )

  def main(args: Array[String]): Unit = {
    val ground = parse(Source.fromResource("input-17.data").getLines())

    val ant = ground.ant()

    while (!ant.done()) ant.walk()

    println(s"solution 1: ${ant.water()}")
    println(s"solution 2: ${ant.restWater()}")
  }
}
