package aoc

object Day5 extends App {
  def solve(modifyPuzzleValue: Int => Int)(puzzle: Vector[Int]) = {
    def move(puzzle: Vector[Int], currentOffset: Int, stepsCount: Int): Int = {
      if (currentOffset >= 0 && currentOffset < puzzle.length) {
        val additionalOffset = puzzle(currentOffset)
        move(puzzle.updated(currentOffset, modifyPuzzleValue(additionalOffset)), currentOffset + additionalOffset, stepsCount + 1)
      } else stepsCount
    }

    move(puzzle, 0, 0)
  }

  val solve1 = solve(_ + 1)(_)
  val solve2 = solve(o => if (o >= 3) o - 1 else o + 1)(_)

  println(s"solve1(...) = ${solve1(Vector(0, 3, 0, 1, -3))}")
  println(s"solve2(...) = ${solve2(Vector(0, 3, 0, 1, -3))}")
}
