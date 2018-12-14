package aoc

object Day14 {
  def recipesMaker(input: Int): Stream[Int] = {
    val inputSize = input.toString.size

    lazy val recipes: Stream[Int] = {
      def loop(firstIndex: Int, secondIndex: Int, size: Int): Stream[Int] = {
        if (size % 10000 == 0)
          println(s"size: $size")

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

  def takeAfter(recipes: Stream[Int], take: Int, after: Int): String =
    recipes.drop(after).take(take).mkString

  def findFirstRecurrence(recipes: Stream[Int], input: String, from: Int = 0): Int =
    recipes
      .sliding(input.size)
      .map { _.mkString }
      .indexOf(input, from)

  def main(args: Array[String]): Unit = {
    val input = 170641

    val recipes = recipesMaker(37)

    //println(s"solution 1: ${takeAfter(recipes, 10, input)}")

    //println(s"solution 2: ${findFirstRecurrence(recipes, input.toString)}")
    //println(s"""solution 2: ${findFirstRecurrence(recipes, "7064")}""")
    val i = findFirstRecurrence(recipes, "706")
    println(i -> takeAfter(recipes, 10, i - 4))

    val j = findFirstRecurrence(recipes, "706", i + 2)
    println(j -> takeAfter(recipes, 10, j - 4))

    val k = findFirstRecurrence(recipes, "706", j + 2)
    println(k -> takeAfter(recipes, 10, k - 4))
  }
}
