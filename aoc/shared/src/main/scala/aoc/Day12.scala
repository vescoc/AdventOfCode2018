package aoc

import scala.annotation.tailrec

object Day12 {
  val initialStateRe = """initial state: ([#.]+)""".r
  val patternRe = """([#.]{5}) => ([#.])""".r

  val compactPotsRe = """([.]*)(#[#.]*#)([.]*)""".r

  def main(args: Array[String]): Unit = {
    val input = if (true) {
      Source
        .fromResource("input-12.data")
        .getLines()
    } else {
      """|initial state: #..#.#..##......###...###
         |
         |...## => #
         |..#.. => #
         |.#... => #
         |.#.#. => #
         |.#.## => #
         |.##.. => #
         |.#### => #
         |#.#.# => #
         |#.### => #
         |##.#. => #
         |##.## => #
         |###.. => #
         |###.# => #
         |####. => #""".stripMargin.lines
    }

    val (Some(initialState), patterns) = input
      .foldLeft[(Option[String], Map[String, String])]((None, Map.empty)) { (acc, l) =>
        (acc._1, l) match {
          case (None, initialStateRe(initialState)) =>
            (Some(initialState), acc._2)
          case (_, patternRe(key, value)) =>
            (acc._1, acc._2 + (key -> value))
          case (_, "") =>
            acc
        }
      }

    case class Generation(pots: String = initialState, zero: Long = 0, compact: Boolean = true) {
      def evolve(): Generation = {
        val tmp = s"...$pots..."
          .sliding(5, 1)
          .map { patterns getOrElse (_, ".") }
          .mkString("")

        if (compact) {
          val (newPots, newZero) = tmp match {
            case compactPotsRe(prefix, compactPots, postfix) =>
              (
                compactPots,
                zero + 1 - prefix.size
              )
          }

          Generation(newPots, newZero, compact)
        } else {
          Generation(tmp, zero + 1, compact)
        }
      }

      def evolve(count: Long): Generation =
        Stream
          .range(1, count + 1)
          .foldLeft(this) { (acc, _) =>
            acc.evolve()
          }

      def value() =
        pots.zipWithIndex
          .filter { _._1 == '#' }
          .map { _._2 - zero }
          .sum
    }

    {
      val generation = Generation().evolve(20)

      println(s"solution 1: ${generation.value()} ($generation)")
    }

    {
      val generation = {
        @tailrec
        def findCycle(
          generation: Generation = Generation(),
          count: Long = 0,
          story: Map[String, (Long, Long)] = Map.empty
        ): (Long, Long, Long) =
          story.get(generation.pots) match {
            case Some((v, zero)) =>
              (count, count - v, generation.zero - zero)
            case _ =>
              findCycle(generation.evolve(), count + 1, story + (generation.pots -> ((count, generation.zero))))
          }

        val (delta, cycle, deltaZero) = findCycle()

        val Target = 50000000000L

        val g = Generation().evolve(delta)

        val r = g
          .copy(zero = g.zero + (Target - delta) * deltaZero / cycle)
          .evolve(Target % cycle)

        if (Target < 100000L)
          assert(Generation().evolve(Target).value() == r.value())

        r
      }

      println(s"solution 2: ${generation.value()} ($generation)")
    }
  }
}
