package aoc

import scala.annotation.tailrec
import scala.util.Try

object Day09 {
  val re = """(\d+) players; last marble is worth (\d+) points""".r

  class Bucket(val marble: Long, var left: Bucket, var right: Bucket) {
    override def toString(): String = {
      @tailrec
      def collect(current: List[Long] = List.empty, bucket: Bucket = right): List[Long] =
        if (bucket == this)
          current.reverse
        else
          collect(bucket.marble :: current, bucket.right)

      s"""Bucket[ ($marble) ${collect().mkString(" ")}]"""
    }
  }
  object Bucket {
    def apply(marble: Long): Bucket = {
      val bucket = new Bucket(marble, null, null)

      bucket.left = bucket
      bucket.right = bucket

      bucket
    }

    def apply(marble: Long, left: Bucket, right: Bucket): Bucket = new Bucket(marble, left, right)
  }

  case class Info(currentBucket: Bucket, billboard: Map[Long, Long])

  def main(args: Array[String]): Unit = {
    val (players, lastMarble, trace) =
      Try {
        (args(0).toLong, args(1).toLong, Try { args(2).toBoolean } getOrElse false)
      }.getOrElse {
        Source
          .fromResource("input-09.data")
          .getLines()
          .map {
            _ match {
              case re(players, lastMarble) => (players.toLong, lastMarble.toLong, false)
            }
          }
          .next
      }

    def solve(Players: Long = players, LastMarble: Long = lastMarble) = {
      val info = Stream
        .range(1L, LastMarble)
        .foldLeft(Info(Bucket(0), Map.empty)) { (acc, marble) =>
          if (trace)
            println(acc)

          val player = marble % Players
          if (marble % 23 == 0) {
            val bucketToRemove = (1 to 7).foldLeft(acc.currentBucket) { (acc, _) =>
              acc.left
            }

            val newCurrentBucket = bucketToRemove.right
            bucketToRemove.left.right = newCurrentBucket
            bucketToRemove.right.left = bucketToRemove.left

            Info(
              newCurrentBucket,
              acc.billboard +
                (player -> (acc.billboard.getOrElse(player, 0L) + (marble + bucketToRemove.marble)))
            )
          } else {
            val targetBucket = acc.currentBucket.right.right

            val bucketToAdd = Bucket(marble, targetBucket.left, targetBucket)

            bucketToAdd.left.right = bucketToAdd
            bucketToAdd.right.left = bucketToAdd

            acc.copy(currentBucket = bucketToAdd)
          }
        }

      info.billboard.maxBy { _._2 }._2
    }

    println(s"solution 1: ${solve()}")

    println(s"solution 2: ${solve(LastMarble = lastMarble * 100)}")
  }
}
