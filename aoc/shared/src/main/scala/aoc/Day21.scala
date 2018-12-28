package aoc

object Day21 {
  import Day19._

  def main(args: Array[String]): Unit = {
    val (Some(cpu), istructions) = CPU.parse(Source.fromResource("input-21.data").getLines())

    val r = cpu.run(Registries(), istructions.take(istructions.size - 3))

    println(s"solution 1: ${cpu.run(Registries(a = r(1)), istructions).a}")

    var solution2 = 0

    val watcher = {
      import scala.collection.mutable

      val count = Iterator.from(0)
      val set = mutable.Set.empty[Int]

      var last = 0

      (pre: Registries, i: Istruction, post: Registries) => {
        if (true) {
          if (pre.pc == 28) {
            val c = count.next
            val value = pre(1)
            if (set.contains(value)) {
              solution2 = last
              false
            } else {
              last = value
              set += value
              if (false && c % 100 == 0)
                println(s"c: $c ${pre} $i ${post}")

              c <= 20000
            }
          } else {
            true
          }
        } else {
          val c = count.next
          println(s"c: $c ${pre} $i ${post}")
          c <= 2000
        }
      }
    }

    cpu.run(Registries(a = 0), istructions, watcher)

    println(s"solution 2: $solution2")
  }
}
