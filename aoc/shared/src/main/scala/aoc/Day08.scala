package aoc

import scala.annotation.tailrec
import scala.util.Try

object Day08 {
  def main(args: Array[String]): Unit = {
    val TailRec = Try { args(0).toBoolean } getOrElse true

    val input = Source
      .fromResource("input-08.data")
      .getLines()
      .flatMap { _.split(" ") }
      .map { _.toInt }
      .toList

    case class Node(children: Int, metadata: Int) {
      def childDone() = copy(children = children - 1)
    }

    def solution1: Int = {
      val iterator = input.toIterator
      if (TailRec) {
        @tailrec
        def parseNodes(sum: Int = 0, stack: List[Node] = List.empty): Int =
          if (iterator.hasNext) {
            stack match {
              case node :: tail =>
                if (node.children == 0) {
                  val newSum = (1 to node.metadata).foldLeft(sum) { (acc, _) =>
                    acc + iterator.next
                  }

                  parseNodes(newSum, tail)
                } else {
                  val children = iterator.next
                  val metadata = iterator.next

                  parseNodes(sum, Node(children, metadata) :: node.childDone() :: tail)
                }
              case _ =>
                val children = iterator.next
                val metadata = iterator.next

                parseNodes(sum, Node(children, metadata) :: stack)
            }
          } else {
            sum
          }

        parseNodes()
      } else {
        def parseNode(): Int = {
          @inline
          def parseNodes(quantity: Int): Int = (1 to quantity).foldLeft(0) { (acc, _) =>
            acc + parseNode()
          }

          val childQuantity = iterator.next
          val metadataQuantity = iterator.next

          (1 to metadataQuantity).foldLeft(parseNodes(childQuantity)) { (acc, _) =>
            acc + iterator.next
          }
        }

        parseNode()
      }
    }

    def solution2: Int = {
      val iterator = input.toIterator

      if (TailRec) {
        @tailrec
        def parseNodes(values: Seq[Int] = List.empty, stack: List[(Node, Seq[Int])] = List.empty): Int =
          if (iterator.hasNext) {
            stack match {
              case (node, list) :: tail =>
                if (node.children == 0) {
                  val checksum =
                    if (values.isEmpty)
                      (1 to node.metadata).foldLeft(0) { (acc, _) =>
                        acc + iterator.next
                      } else
                      (1 to node.metadata).foldLeft(0) { (acc, _) =>
                        val index = iterator.next - 1
                        acc + (Try { values(index) } getOrElse 0)
                      }

                  parseNodes(list ++ List(checksum), tail)
                } else {
                  val children = iterator.next
                  val metadata = iterator.next

                  parseNodes(List.empty, (Node(children, metadata), values) :: (node.childDone(), list) :: tail)
                }
              case _ =>
                val children = iterator.next
                val metadata = iterator.next

                parseNodes(List.empty, (Node(children, metadata), values) :: stack)
            }
          } else {
            values(0)
          }

        parseNodes()
      } else {
        def parseNode(): Int = {
          def parseNodes(quantity: Int): List[Int] =
            (1 to quantity)
              .foldLeft(List.empty[Int]) { (acc, _) =>
                parseNode() :: acc
              }
              .reverse

          val childQuantity = iterator.next
          val metadataQuantity = iterator.next

          childQuantity match {
            case 0 =>
              (1 to metadataQuantity).foldLeft(0) { (acc, _) =>
                acc + iterator.next
              }
            case _ =>
              val children = parseNodes(childQuantity)
              (1 to metadataQuantity).foldLeft(0) { (acc, _) =>
                val index = iterator.next - 1
                acc + (Try { children(index) } getOrElse 0)
              }
          }
        }

        parseNode()
      }
    }

    println(s"solution 1: $solution1")
    println(s"solution 2: $solution2")
  }
}
