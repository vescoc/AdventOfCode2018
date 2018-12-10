package aoc

import scala.annotation.tailrec

object Day07 {
  val TRACE = false

  val StepRe = """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r

  case class Worker(workingOn: Option[(Char, Int)] = None) {
    def assign(time: Int, work: Char) = Worker(Some((work, time + work - 'A' + 1 + 60)))

    def tick(time: Int) =
      workingOn match {
        case None =>
          (None, this)
        case Some((value, endTime)) =>
          if (time >= endTime)
            (Some(value), Worker())
          else
            (None, this)
      }

    override def toString() =
      workingOn match {
        case Some((c, t)) => s"Worker($c,s=${t - (c - 'A' + 1 + 60)},e=$t)"
        case None         => "Worker()"
      }
  }

  def main(args: Array[String]): Unit = {
    val dependencies = Source
      .fromResource("input-07.data")
      .getLines()
      .foldLeft(Map.empty[Char, Set[Char]]) { (acc, line) =>
        line match {
          case StepRe(master, slave) =>
            acc + (slave(0) -> (acc.getOrElse(slave(0), Set.empty) + master(0))) + (master(0) -> acc.getOrElse(
              master(0),
              Set.empty
            ))
        }
      }

    @tailrec
    def solve(
      time: Int = 0,
      dependencies: Map[Char, Set[Char]] = dependencies,
      steps: String = "",
      idle: List[Worker] = List.fill(1) { Worker() },
      working: List[Worker] = List.empty
    ): (String, Int) =
      if (dependencies.isEmpty && working.isEmpty)
        (steps, time - 1)
      else {
        @tailrec
        def doWork(
          dependencies: Map[Char, Set[Char]] = dependencies,
          working: List[Worker] = working,
          idle: List[Worker] = idle,
          newWorking: List[Worker] = List.empty,
          newIdle: List[Worker] = List.empty,
          completedSteps: Set[Char] = Set.empty
        ): (Map[Char, Set[Char]], Set[Char], List[Worker], List[Worker]) =
          working match {
            case worker :: workers =>
              worker.tick(time) match {
                case (Some(step), w) =>
                  val newDependencies = ((dependencies - step).map { p =>
                    p._1 -> (p._2 - step)
                  })
                  doWork(newDependencies, workers, w :: idle, newWorking, newIdle, completedSteps + step)
                case (None, w) =>
                  doWork(dependencies, workers, idle, w :: newWorking, newIdle, completedSteps)
              }
            case Nil =>
              if (dependencies.isEmpty)
                (dependencies, completedSteps, newWorking, newIdle ++ idle)
              else
                idle match {
                  case h :: t =>
                    val running = newWorking
                      .map { _.workingOn }
                      .collect { case Some((step, _)) => step }

                    val steps = dependencies
                      .filter { _._2.isEmpty }
                      .map { _._1 }
                      .toSet -- running

                    if (steps.isEmpty)
                      (dependencies, completedSteps, newWorking, newIdle ++ idle)
                    else
                      doWork(dependencies, Nil, t, h.assign(time, steps.min) :: newWorking, newIdle, completedSteps)
                  case Nil =>
                    (dependencies, completedSteps, newWorking, newIdle)
                }
          }

        val (newDependencies, completedSteps, newWorking, newIdle) = doWork()

        if (TRACE) {
          println(s"time: $time")
          println(s"  dependencies: $dependencies")
          println(s"  steps: $steps")
          println(s"  working: $working")
          println(s"  idle: $idle")
          println(s"  newDependencies: $newDependencies")
          println(s"  completedSteps: $completedSteps")
          println(s"  newWorking: $newWorking")
          println(s"  newIdle: $newIdle")
        }

        val step = completedSteps.toList.sortBy { _.toInt }.mkString

        solve(
          time + 1,
          newDependencies,
          steps + step,
          newIdle,
          newWorking
        )
      }

    println(s"solution 1: ${solve()._1}")

    println(s"solution 2: ${solve(idle = List.fill(6) { Worker() })._2}")
  }
}
