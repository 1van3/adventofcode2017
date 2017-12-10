package aoc

object Day2 extends App {
 def minMax(line: Seq[Int]) =
   line.foldLeft((line(0), line(0))) { case ((min, max), el) =>
     (Math.min(min, el), Math.max(max, el))
   }
 
 def solve(processLine: Seq[Int] => Int)(puzzle: Seq[Seq[Int]]) = puzzle.map(processLine).sum

 val solve1 =
   solve { line =>
     val (min, max) = minMax(line)
     max - min
   }(_)

 val solve2 = {
   def findModZero(line: Seq[Int]) =
     line.combinations(2)
       .collectFirst { case Seq(left, right) if left % right == 0 || right % left == 0 =>
         minMax(Seq(left, right)).swap
       }

   solve { line =>
     findModZero(line) match {
       case Some((left, right)) => left / right
       case _ => 0
     }
   }(_)
 }

 val inp = Seq(Seq(5, 9, 2, 8), Seq(9, 4, 7, 3), Seq(3, 8, 6, 5))
 println(s"solve(...) = ${solve2(inp)}")
}
