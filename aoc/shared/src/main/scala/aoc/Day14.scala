package aoc

object Day14 {
  def recipesMaker(input: Int): Stream[Int] = {
    val inputSize = input.toString.size

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

      input.toString
        .split("")
        .map { _.toInt }
        .toStream #::: loop(0, 1, inputSize)
    }

    recipes
  }

  def main(args: Array[String]): Unit = {
    val input = 170641

    val recipes = recipesMaker(37)

    println(s"""solution 1: ${recipes.drop(input).take(10).mkString}""")
  }
}
