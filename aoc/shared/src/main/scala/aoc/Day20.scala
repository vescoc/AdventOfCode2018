package aoc

import scala.annotation.tailrec

object Day20 {
  object MapElement extends Enumeration {
    type MapElement = Char

    val Unknown = '?'
    val HorizontalDoor = '-'
    val VerticalDoor = '|'
    val Room = '.'
    val Wall = '#'
  }

  import MapElement._

  case class RegularMap(position: (Int, Int), map: Map[(Int, Int), MapElement]) {
    lazy val (min, max) = {
      implicit class HandleTuple2(t: (Int, Int)) {
        def min(that: (Int, Int)) = (Math.min(t._1, that._1), Math.min(t._2, that._2))
        def max(that: (Int, Int)) = (Math.max(t._1, that._1), Math.max(t._2, that._2))
      }

      map
        .foldLeft(((Int.MaxValue, Int.MaxValue), (Int.MinValue, Int.MinValue))) { (acc, kv) =>
          (acc._1 min kv._1, acc._2 max kv._1)
        }
    } 

    override def toString: String =
      (
        for {
          y <- min._2 to max._2
        } yield {
          (
            for {
              x <- min._1 to max._1
              p = (x, y)
            } yield {
              if (position == p)
                'X'
              else
                map.getOrElse((x, y), ' ')
            }
          )
            .map { c =>
              c match {
                case Unknown => Wall
                case _ => c
              }
            }
            .mkString
        }
      )
        .mkString("\n")
  }

  def furthest(path: String): Int = {
    val i = path.toIterator

    @tailrec
    def count(current: Int, list: List[Int], stack: List[List[Int]]): Int =
      i.next match {
        case '^' if stack.isEmpty && list.isEmpty =>
          count(0, list, stack)
        case '$' if stack.isEmpty && list.isEmpty =>
          current
        case 'N' | 'S' | 'E' | 'W' =>
          count(current + 1, list, stack)
        case '(' =>
          count(0, List.empty, (current :: list) :: stack)
        case ')' =>
          stack match {
            case head :: tail =>
              head match {
                case last :: newList =>
                  count(
                    last + {
                      val l = current :: list
                      if (l.contains(0))
                        0
                      else
                        l.max
                    },
                    newList,
                    tail
                  )
                case _ =>
                  throw new MatchError(head)
              }
            case _ =>
              throw new MatchError(stack)
          }
        case '|' =>
          count(0, current :: list, stack)
      }

    count(0, List.empty, List.empty)
  }

  def main(args: Array[String]): Unit = {
    val input = Source
      .fromResource("input-20.data")
      .getLines()
      .mkString

    println(s"solution 1: ${furthest(input)}")
  }
}
