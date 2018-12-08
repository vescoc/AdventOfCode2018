package aoc

import scala.io.Source

object Day08 {
  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("input-08.data")
      .getLines()
      .flatMap { _.split(" ") }
      .map { _.toInt }
      .toList

    def solution1: Int = {
      val iterator = input.toIterator

      def parseNode(): Int = {
        @inline
        def parseNodes(quantity: Int): Int = (1 to quantity).foldLeft(0) { (acc, _) => acc + parseNode() }

        val childQuantity = iterator.next
        val metadataQuantity = iterator.next

        parseNodes(childQuantity) + (1 to metadataQuantity).foldLeft(0) { (acc, _) => acc + iterator.next }
      }

      parseNode()
    }

    def solution2: Int = {
      val iterator = input.toIterator

      def parseNode(): Int = {
        def parseNodes(quantity: Int): List[Int] =
          (1 to quantity)
            .foldLeft(List.empty[Int]) { (acc, _) => parseNode() :: acc }
            .reverse

        val childQuantity = iterator.next
        val metadataQuantity = iterator.next

        childQuantity match {
          case 0 =>
            (1 to metadataQuantity).foldLeft(0) { (acc, _) => acc + iterator.next }
          case _ =>
            val children = parseNodes(childQuantity)
            (1 to metadataQuantity).foldLeft(0) { (acc, _) =>
              val index = iterator.next - 1
              val value = if (index >= 0 && index < children.size) children(index) else 0
              acc + value
            }
        }
      }

      parseNode()
    }

    println(s"solution 1: $solution1")
    println(s"solution 2: $solution2")
  }
}
