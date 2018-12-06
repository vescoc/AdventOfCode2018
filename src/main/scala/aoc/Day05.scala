package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day05 {
  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("input-05.data")
      .mkString
      .trim

    val delta = 'a' - 'A'

    def react(input: String = input): Int = {
      val buffer = new StringBuilder(input)

      @tailrec
      def analize(index: Int = 0): Unit =
        if (index < buffer.size - 1) {
          if (Math.abs(buffer.charAt(index) - buffer.charAt(index + 1)) == delta) {
            buffer.deleteCharAt(index)
            buffer.deleteCharAt(index)

            analize(
              if (index - 1 < 0)
                0
              else
                index - 1
            )
          } else
            analize(index + 1)
        }

      analize()

      buffer.size
    }

    println(s"solution 1: ${react()}")

    val solve2 = {
      for {
        c <- 'A' to 'Z'
      } yield {
        val newInput = input
          .replace(c.toChar.toString, "")
          .replace((c + delta).toChar.toString, "")

        react(newInput)
      }
    }.min

    println(s"solution 2: $solve2")
  }
}
