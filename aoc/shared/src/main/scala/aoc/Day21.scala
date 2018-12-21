package aoc

object Day21 {
  import Day19._

  def main(args: Array[String]): Unit = {
    val (Some(cpu), istructions) = CPU.parse(Source.fromResource("input-21.data").getLines())

    val r = cpu.run(Registries(), istructions.take(istructions.size - 3))

    println(s"solution 1: ${r(1)}")
  }
}
