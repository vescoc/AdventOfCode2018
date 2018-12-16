package aoc

import scala.annotation.tailrec

object Day16 {
  val beforeRe = """Before:\s+\[(\d+),\s+(\d+),\s+(\d+),\s+(\d+)\]""".r
  val afterRe = """After:\s+\[(\d+),\s+(\d+),\s+(\d+),\s+(\d+)\]""".r
  val istructionRe = """(\d+)\s+(\d+)\s+(\d+)\s+(\d+)""".r

  case class Istruction(opcode: Int, a: Int, b: Int, c: Int)

  case class Registries(a: Int, b: Int, c: Int, d: Int) {
    def apply(n: Int) =
      n match {
        case 0 => a
        case 1 => b
        case 2 => c
        case 3 => d
      }

    def update(n: Int, value: Int) =
      n match {
        case 0 => copy(a = value)
        case 1 => copy(b = value)
        case 2 => copy(c = value)
        case 3 => copy(d = value)
      }
  }

  sealed trait OpCode {
    def name: String

    def run(r: Registries, i: Istruction): Registries
  }

  object OpCodes {
    lazy val all = List(
      addr,
      addi,
      mulr,
      muli,
      banr,
      bani,
      borr,
      bori,
      setr,
      seti,
      gtir,
      gtri,
      gtrr,
      eqir,
      eqri,
      eqrr
    )

    val addr: OpCode = AOpCode("addr", (r, i) => r(i.c) = r(i.a) + r(i.b))
    val addi: OpCode = AOpCode("addi", (r, i) => r(i.c) = r(i.a) + i.b)
    val mulr: OpCode = AOpCode("mulr", (r, i) => r(i.c) = r(i.a) * r(i.b))
    val muli: OpCode = AOpCode("muli", (r, i) => r(i.c) = r(i.a) * i.b)
    val banr: OpCode = AOpCode("banr", (r, i) => r(i.c) = r(i.a) & r(i.b))
    val bani: OpCode = AOpCode("bani", (r, i) => r(i.c) = r(i.a) & i.b)
    val borr: OpCode = AOpCode("borr", (r, i) => r(i.c) = r(i.a) | r(i.b))
    val bori: OpCode = AOpCode("bori", (r, i) => r(i.c) = r(i.a) | i.b)
    val setr: OpCode = AOpCode("setr", (r, i) => r(i.c) = r(i.a))
    val seti: OpCode = AOpCode("seti", (r, i) => r(i.c) = i.a)
    val gtir: OpCode = AOpCode("gtir", (r, i) => r(i.c) = if (i.a > r(i.b)) 1 else 0)
    val gtri: OpCode = AOpCode("gtri", (r, i) => r(i.c) = if (r(i.a) > i.b) 1 else 0)
    val gtrr: OpCode = AOpCode("gtrr", (r, i) => r(i.c) = if (r(i.a) > r(i.b)) 1 else 0)
    val eqir: OpCode = AOpCode("gtir", (r, i) => r(i.c) = if (i.a == r(i.b)) 1 else 0)
    val eqri: OpCode = AOpCode("gtri", (r, i) => r(i.c) = if (r(i.a) == i.b) 1 else 0)
    val eqrr: OpCode = AOpCode("gtrr", (r, i) => r(i.c) = if (r(i.a) == r(i.b)) 1 else 0)

    private[OpCodes] case class AOpCode(val name: String, f: (Registries, Istruction) => Registries) extends OpCode {
      def run(r: Registries, i: Istruction) = f(r, i)

      override def toString = name
    }
  }

  case class Sample(before: Registries, istruction: Istruction, after: Registries) {
    def test(): Seq[OpCode] =
      OpCodes.all
        .filter { opcode =>
          opcode.run(before, istruction) == after
        }
  }

  case class Data(samples: List[Sample], istructions: List[Istruction]) {
    def associate() = {
      @tailrec
      def associate(map: Map[Int, OpCode], samples: Set[Sample]): Map[Int, OpCode] =
        if (samples.isEmpty) {
          assert(map.size == OpCodes.all.size)

          map
        } else {
          val mappedOps = map.values.toSet

          val uniqueSamples = samples
            .map { sample =>
              sample -> (
                sample.test().toSet -- mappedOps
              )
            }
            .filter { p =>
              p._2.size == 1
            }

          assert(uniqueSamples.size > 0)

          val foundOpCodes = uniqueSamples
            .groupBy { p =>
              (p._1.istruction.opcode, p._2.iterator.next)
            }
            .map { _._1 }
            .toMap

          assert(foundOpCodes.size > 0)

          associate(
            map ++ foundOpCodes,
            samples -- uniqueSamples.map { _._1 }
          )
        }

      associate(Map.empty, samples.toSet)
    }

    def run(map: Map[Int, OpCode]) =
      istructions.foldLeft(Registries(0, 0, 0, 0)) { (acc, i) =>
        map(i.opcode).run(acc, i)
      }
  }

  def parse(lines: Iterator[String]): Data = {
    val d = lines
      .foldLeft[(List[Sample], List[Istruction], Option[Registries], Option[Istruction])](
        (List.empty[Sample], List.empty[Istruction], None, None)
      ) { (acc, line) =>
        (line, acc._3, acc._4) match {
          case (istructionRe(a, b, c, d), None, None) =>
            (
              acc._1,
              Istruction(a.toInt, b.toInt, c.toInt, d.toInt) :: acc._2,
              None,
              None
            )
          case (beforeRe(a, b, c, d), None, None) =>
            (
              acc._1,
              acc._2,
              Some(Registries(a.toInt, b.toInt, c.toInt, d.toInt)),
              None
            )
          case (istructionRe(a, b, c, d), Some(before), None) =>
            (
              acc._1,
              acc._2,
              Some(before),
              Some(Istruction(a.toInt, b.toInt, c.toInt, d.toInt))
            )
          case (afterRe(a, b, c, d), Some(before), Some(istruction)) =>
            (
              Sample(before, istruction, Registries(a.toInt, b.toInt, c.toInt, d.toInt)) :: acc._1,
              acc._2,
              None,
              None
            )
          case ("", None, None) =>
            // ok
            acc
        }
      }

    Data(d._1.reverse, d._2.reverse)
  }

  def main(args: Array[String]): Unit = {
    val data = parse(Source.fromResource("input-16.data").getLines())

    val solution1 =
      data.samples
        .map { sample =>
          sample.test().size
        }
        .filter { _ >= 3 }
        .size

    println(s"solution 1: $solution1")

    val map = data.associate()

    val solution2 = data.run(map)

    println(s"solution 2: ${solution2.a}")
  }
}
