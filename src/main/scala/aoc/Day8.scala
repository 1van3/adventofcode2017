package aoc

import scala.io.Source

object Day8 extends App {
  type Register = String

  sealed trait Operation
  case object Increase extends Operation
  case object Decrease extends Operation

  sealed trait Operator
  case object Equal extends Operator
  case object NotEqual extends Operator
  case object Greater extends Operator
  case object Less extends Operator
  case object GreaterOrEqual extends Operator
  case object LessOrEqual extends Operator

  case class Condition(register: Register, value: Int, op: Operator)
  case class Instruction(register: Register, op: Operation, value: Int, condition: Condition)

  val instructioPattern =
    "([A-Za-z]+) (inc|dec) (\\-*[0-9]+) if ([A-Za-z]+) (>|<|==|!=|>=|<=) (\\-*[0-9]+)".r("reg", "op", "val", "cReg", "cOp", "cVal")

  def parse(line: String): Instruction =
    instructioPattern.findAllMatchIn(line).map { m =>
      def parseOperation(op: String) = op match {
        case "dec" => Decrease
        case "inc" => Increase
      }

      def parseOperator(op: String) = op match {
        case ">" => Greater
        case "<" => Less
        case "==" => Equal
        case "!=" => NotEqual
        case ">=" => GreaterOrEqual
        case "<=" => LessOrEqual
      }

      val reg = m.group("reg")
      val op = parseOperation(m.group("op"))
      val value = m.group("val").toInt
      val cReg = m.group("cReg")
      val cOp = parseOperator(m.group("cOp"))
      val cVal = m.group("cVal").toInt

      Instruction(reg, op, value, Condition(cReg, cVal, cOp))
    }.toSeq.head

  def eval(registers: Map[String, Int], instruction: Instruction) = {
    def evalValue(left: Int, right: Int, op: Operation): Int = op match {
      case Increase => left + right
      case Decrease => left - right
    }

    def evalCondition(left: Int, right: Int, op: Operator): Boolean = op match {
      case Equal => left == right
      case NotEqual => left != right
      case Greater => left > right
      case Less => left < right
      case GreaterOrEqual => left >= right
      case LessOrEqual => left <= right
    }

    val registerValue = registers.getOrElse(instruction.register, 0)
    val condRegisterValue = registers.getOrElse(instruction.condition.register, 0)

    if (evalCondition(condRegisterValue, instruction.condition.value, instruction.condition.op)) {
      registers.updated(instruction.register, evalValue(registerValue, instruction.value, instruction.op))
    } else registers
  }

  def solve1(puzzle: Seq[Instruction]) = {
    def evalList(registers: Map[String, Int], instructions: Seq[Instruction]): Map[String, Int] =
      instructions match {
        case h :: xs => evalList(eval(registers, h), xs)
        case _ => registers
      }

    evalList(Map.empty, puzzle).values.max
  }

  def solve2(puzzle: Seq[Instruction]) = {
    def findMaxDuringProcess(registers: Map[String, Int], instructions: Seq[Instruction], latestMax: Int): Int =
      instructions match {
        case h :: xs =>
          val newRegisters = eval(registers, h)
          val maxInRegisters = if (newRegisters.nonEmpty) newRegisters.values.max else 0
          findMaxDuringProcess(newRegisters, xs, Math.max(maxInRegisters, latestMax))
        case _ => latestMax
      }

    findMaxDuringProcess(Map.empty, puzzle, 0)
  }

  println(solve2(Source.fromFile("input_file_path").getLines.map(parse).toList))
}
