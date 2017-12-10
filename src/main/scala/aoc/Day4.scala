package aoc

import scala.io.Source

object Day4 extends App {
  def solve1(passphrase: String) = {
    val tokens = passphrase.split(" ")
    tokens.toSet.size == tokens.length
  }

  def solve2(passphrase: String) = {
    val tokens = passphrase.split(" ")
    tokens
      .map { token =>
        token.groupBy(identity).map { case (c, group) => (c, group.length) }
      }
      .toSet.size == tokens.length
  }

  println(Source.fromFile("input_file_path").getLines.count(solve2))
}
