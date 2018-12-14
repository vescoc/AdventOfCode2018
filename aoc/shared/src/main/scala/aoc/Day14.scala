package aoc

import scala.annotation.tailrec

object Day14 {
  def recipesMaker(streamImpl: Boolean = false): ((Int, Int) => String, (String) => Int) =
    if (streamImpl) {
      lazy val recipes: Stream[Int] = {
        def loop(firstIndex: Int, secondIndex: Int, size: Int): Stream[Int] = {
          val first = recipes(firstIndex)
          val second = recipes(secondIndex)

          val sum = first + second

          if (sum >= 10) {
            val newSize = size + 2
            (sum / 10) #:: (sum % 10) #:: loop(
              (firstIndex + 1 + first) % newSize,
              (secondIndex + 1 + second) % newSize,
              newSize
            )
          } else {
            val newSize = size + 1
            sum #:: loop((firstIndex + 1 + first) % newSize, (secondIndex + 1 + second) % newSize, newSize)
          }
        }

        3 #:: 7 #:: loop(0, 1, 2)
      }

      def takeAfter(take: Int, after: Int): String =
        recipes.drop(after).take(take).mkString

      def findFirstRecurrence(input: String): Int =
        recipes
          .sliding(input.size)
          .map { _.mkString }
          .indexOf(input)

      (takeAfter, findFirstRecurrence)
    } else {
      val recipes = Array.ofDim[Byte](100 * 1000 * 1000)

      recipes(0) = 3
      recipes(1) = 7

      var size = 2

      var firstIndex = 0
      var secondIndex = 1

      def calc(): Unit = {
        val first = recipes(firstIndex)
        val second = recipes(secondIndex)

        val sum = first + second

        if (sum >= 10) {
          recipes(size) = (sum / 10).toByte
          size += 1
        }

        recipes(size) = (sum % 10).toByte
        size += 1

        firstIndex = (firstIndex + 1 + first) % size
        secondIndex = (secondIndex + 1 + second) % size
      }

      def takeAfter(take: Int, after: Int): String = {
        while (size < after + take)
          calc()

        recipes.drop(after).take(take).map { _.toString }.mkString
      }

      def findFirstRecurrence(input: String): Int = {
        val bytes = input
          .split("")
          .map { _.toInt }

        val bytesSize = bytes.size

        @tailrec
        def find(index: Int): Int =
          if (index + bytesSize > size) {
            calc()
            find(index)
          } else if ((0 until bytesSize).forall { i => bytes(i) == recipes(index + i) })
            index
          else {
            find(index + 1)
          }

        find(0)
      }

      (takeAfter, findFirstRecurrence)
    }

  def main(args: Array[String]): Unit = {
    val input = 170641

    val (takeAfter, findFirstRecurrence) = recipesMaker()

    println(s"solution 1: ${takeAfter(10, input)}")

    println(s"solution 2: ${findFirstRecurrence(input.toString)}")
  }
}
