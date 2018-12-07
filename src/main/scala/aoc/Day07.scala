package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day07 {
  val StepRe = """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r

  case class Worker(workingOn: Option[(Char, Int)] = None) {
    def assign(workingOn: (Char, Int)) = Worker(Some(workingOn))

    def tick(time: Int) =
      workingOn match {
        case None =>
          (None, this)
        case Some((value, startTime)) =>
          if (startTime + time > 60 + (value.toInt - 'A' + 1))
            (Some(value), Worker())
          else
            (None, this)
      }
  }

  def main(args: Array[String]): Unit = {
    val dependencies = Source
      .fromResource("input-07.data")
      .getLines()
      .foldLeft(Map.empty[Char, Set[Char]]) { (acc, line) =>
        line match {
          case StepRe(master, slave) =>
            acc + (slave(0) -> (acc.getOrElse(slave(0), Set.empty) + master(0))) + (master(0) -> acc.getOrElse(master(0), Set.empty))
        }
      }

    @tailrec
    def solve(
      time: Int = 0,
      dependencies: Map[Char, Set[Char]] = dependencies,
      steps: String = "",
      idle: List[Worker] = List.fill(1) { Worker() },
      working: List[Worker] = List.empty): String =
      if (dependencies.isEmpty && working.isEmpty)
        steps
      else {
        @tailrec
        def doWork(
          dependencies: Map[Char, Set[Char]] = dependencies,
          working: List[Worker] = working,
          idle: List[Worker] = idle,
          newWorking: List[Worker] = List.empty,
          newIdle: List[Worker] = List.empty,
          completedSteps: Set[Char] = Set.empty
        ): (Set[Char], List[Worker], List[Worker]) = {
          working match {
            case worker :: workers =>
              worker.tick(time) match {
                case (Some(value), w) =>
                  doWork(dependencies, workers, w :: idle, newWorking, newIdle, completedSteps + value)
                case (None, w) =>
                  doWork(dependencies, workers, idle, w :: newWorking, newIdle, completedSteps)
              }
          }
        }

        val (completedSteps, newWorking, newIdle) = doWork()

        val step = dependencies
          .filter { _._2.isEmpty }
          .map { _._1 }
          .min

        solve(
          time + 1,
          ((dependencies - step).map { p => p._1 -> (p._2 - step) }),
          steps + step,
          idle,
          working
        )
      }

    println(s"solution 1: ${solve()}")
  }
}
