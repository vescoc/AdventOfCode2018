package aoc

import scala.io.Source

object Day04 {
  val GuardBeginShiftRe = """\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] Guard #(\d+) begins shift""".r
  val FallsAsleepRe = """\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] falls asleep""".r
  val WakesUpRe = """\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] wakes up""".r

  case class Date(year: Int = 0, month: Int = 0, day: Int = 0)
  object Date {
    def apply(year: String, month: String, day: String): Date = Date(year.toInt, month.toInt, day.toInt)
  }

  case class Time(hour: Int = 0, minute: Int = 0)
  object Time {
    def apply(hour: String, minute: String): Time = Time(hour.toInt, minute.toInt)
  }

  case class Info(minutes: Array[Int] = new Array(60)) {
    def totalTimeAsleep = minutes.sum

    override def toString =
      s"""Info($totalTimeAsleep, ${minutes.mkString("[", ",", "]")})"""
  }

  type Infos = Map[Int, Info]

  case class State(id: Int = 0, date: Date = Date(), time: Time = Time(), asleep: Boolean = false) {
    def guardBeginShift(id: Int, date: Date, time: Time): State = {
      assert(this.asleep == false, s"invalid asleep state for $this, guardBeginShift at $date $time: expected false")
      State(id, date, time, false)
    }

    def fallsAsleep(date: Date, time: Time): State = {
      assert(this.asleep == false, s"invalid asleep state for $this, fallsAsleep at $date $time: expected false")

      copy(date = date, time = time, asleep = true)
    }

    def wakesUp(date: Date, time: Time, infos: Infos): (State, Infos) = {
      assert(this.asleep, s"invalid asleep state for $this, wakesUp at $date $time: expected true")

      val info = infos.getOrElse(id, Info())

      val minutes = info.minutes.zipWithIndex
        .map { v =>
          val (value, index) = v
          if (index >= this.time.minute && index < time.minute)
            value + 1
          else
            value
        }
      (copy(date = date, time = time, asleep = false), infos + (id -> Info(minutes)))
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("input-04.data")
      .getLines()
      .toList
      .sorted

    val (_, infos) = input
      .foldLeft((State(), Map.empty[Int, Info])) { (acc, command) =>
        val (state, infos) = acc

        command match {
          case GuardBeginShiftRe(year, month, day, hour, minute, id) =>
            (state.guardBeginShift(id.toInt, Date(year, month, day), Time(hour, minute)), infos)
          case FallsAsleepRe(year, month, day, hour, minute) =>
            (state.fallsAsleep(Date(year, month, day), Time(hour, minute)), infos)
          case WakesUpRe(year, month, day, hour, minute) =>
            state.wakesUp(Date(year, month, day), Time(hour, minute), infos)
        }
      }

    {
      val info = infos.maxBy { p =>
        p._2.totalTimeAsleep
      }

      println(s"solution 1: ${info._1 * info._2.minutes.indexOf(info._2.minutes.max)}")
    }

    {
      val info = infos.maxBy { p =>
        p._2.minutes.max
      }

      println(s"solution 2: ${info._1 * info._2.minutes.indexOf(info._2.minutes.max)}")
    }
  }
}
