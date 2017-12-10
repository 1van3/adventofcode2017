package aoc

object Day1 extends App {
 def solve(puzzle: String)(nextDistance: Int) = {
   def s(puzzle: String) =
     puzzle.take(puzzle.length / 2).zipWithIndex.collect {
       case (curr, i) if curr == puzzle(i + nextDistance) => curr
     }
     .map(_.toString.toInt)
     .sum

   s(puzzle + puzzle)
 }

 println(s"solve(1122)(1) = ${solve("1122")(1)}")
 println(s"solve(1122)(4 / 2) = ${solve("1212")(4 / 2)}")
}
