package aoc

import scala.annotation.tailrec

object Day19 {
  val ipRe = """#ip (\d)""".r
  val istructionRe = """([a-z]+) (\d+) (\d+) (\d+)""".r

  case class Istruction(opcode: OpCode, a: Int, b: Int, c: Int) {
    def run(r: Registries): Registries = opcode.run(r, this)

    override def toString = s"$opcode $a $b $c"
  }

  case class Registries(
    pc: Int = 0,
    a: Int = 0,
    b: Int = 0,
    c: Int = 0,
    d: Int = 0,
    e: Int = 0,
    f: Int = 0
  ) {
    def apply(n: Int) =
      n match {
        case 0 => a
        case 1 => b
        case 2 => c
        case 3 => d
        case 4 => e
        case 5 => f
      }

    def update(n: Int, value: Int) =
      n match {
        case 0 => copy(a = value)
        case 1 => copy(b = value)
        case 2 => copy(c = value)
        case 3 => copy(d = value)
        case 4 => copy(e = value)
        case 5 => copy(f = value)
      }

    def updatePc(n: Int) = copy(pc = apply(n) + 1)

    override def toString = s"ip=$pc [$a, $b, $c, $d, $e, $f]"
  }

  case class CPU(rip: Int) {
    def run(r: Registries, istruction: Istruction): Registries = {
      val result = istruction.run((r(rip) = r.pc)).updatePc(rip)

      // println(s"$r $istruction $result")

      result
    }

    def run(r: Registries, istructions: List[Istruction]): Registries = {
      @tailrec
      def execute(r: Registries): Registries =
        if (r.pc < 0 || r.pc >= istructions.size)
          r
        else
          execute(run(r, istructions(r.pc)))

      execute(r)
    }
  }
  object CPU {
    def parse(lines: Iterator[String]): (Option[CPU], List[Istruction]) =
      lines
        .foldLeft[(Option[CPU], List[Istruction])]((None, List.empty)) { (acc, line) =>
          (acc, line) match {
            case ((None, l), ipRe(rip)) if l.isEmpty =>
              (Some(CPU(rip.toInt)), l)
            case ((Some(cpu), l), istructionRe(opcode, a, b, c)) =>
              (Some(cpu), l :+ Istruction(OpCodes.map(opcode), a.toInt, b.toInt, c.toInt))
          }
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

    lazy val map = all.map { opcode => opcode.name -> opcode }.toMap

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
    val eqir: OpCode = AOpCode("eqir", (r, i) => r(i.c) = if (i.a == r(i.b)) 1 else 0)
    val eqri: OpCode = AOpCode("eqri", (r, i) => r(i.c) = if (r(i.a) == i.b) 1 else 0)
    val eqrr: OpCode = AOpCode("eqrr", (r, i) => r(i.c) = if (r(i.a) == r(i.b)) 1 else 0)

    private[OpCodes] case class AOpCode(val name: String, f: (Registries, Istruction) => Registries) extends OpCode {
      def run(r: Registries, i: Istruction) = f(r, i)

      override def toString = name
    }
  }

  def main(args: Array[String]): Unit = {
    val (Some(cpu), istructions) = CPU.parse(Source.fromResource("input-19.data").getLines())

    val solution1 = cpu.run(Registries(), istructions)

    println(s"solution 1: $solution1")

    /*
     |0=a, 1=b, 2=c, 3=d, 4=e, 5=f
     |#ip 2
     |0: addi 2 16 2 | jp 0 + 16 + 1 (17)
     |1: seti 1 0 1 | b = 1
     |2: seti 1 4 3 | d = 1
     |3: mulr 1 3 4 | e = b * d
     |4: eqrr 4 5 4 | e = if e == f
     |5: addr 4 2 2 | jp e + 5 + 1 (6 / 7 e)
     |6: addi 2 1 2 | jp 6 + 1 + 1 (8)
     |7: addr 1 0 0 | a = b + a
     |8: addi 3 1 3 | d = d + 1
     |9: gtrr 3 5 4 | e = if d > f
     |10: addr 2 4 2 | jp 10 + e + 1 (11 / 12 e)
     |11: seti 2 5 2 | jp 2 + 1 (3)
     |12: addi 1 1 1 | b = b + 1
     |13: gtrr 1 5 4 | e = if b > f
     |14: addr 4 2 2 | jp e + 14 + 1 (15 / 16 e)
     |15: seti 1 1 2 | jp 1 + 1 (2)
     |16: mulr 2 2 2 | jp 16 * 16 + 1
     |17: addi 5 2 5 | f = 17 + f
     |18: mulr 5 5 5 | f = f * f
     |19: mulr 2 5 5 | f = 19 * f
     |20: muli 5 11 5 | f = f * 11
     |21: addi 4 5 4 | e = e + 5
     |22: mulr 4 2 4 | e = e * 22
     |23: addi 4 9 4 | e = e + 9
     |24: addr 5 4 5 | f = f + e
     |25: addr 2 0 2 | jp 25 + a + 1 (26 + a)
     |26: seti 0 0 2 | jp 0 + 1 (1)
     |27: setr 2 3 4 | e = 27 + d
     |28: mulr 4 2 4 | e = e * 28
     |29: addr 2 4 4 | e = 29 + e
     |30: mulr 2 4 4 | e = 30 * e
     |31: muli 4 14 4 | e = e * 14
     |32: mulr 4 2 4 | e = 32 * e
     |33: addr 5 4 5 | f = f + e
     |34: seti 0 6 0 | a = 0
     |35: seti 0 3 2 | jp 0 + 1 (1)
     */

    //val solution2 = cpu.run(Registries(a = 1), istructions)
  }
}
