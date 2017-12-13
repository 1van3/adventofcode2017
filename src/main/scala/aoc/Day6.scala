package aoc

object Day6 extends App {
  type Memory = Vector[Int]

  def solve(memory: Memory) = {
    def reallocate(memory: Memory): Memory = {
      def reallocate(memory: Memory, numBlocks: Int, startIndex: Int): Memory = {
        val banksToUpdate =
          (0 until numBlocks)
            .map { opIndex => (startIndex + opIndex + 1) % memory.length }
            .groupBy(identity)
            .map { case (i, g) => i -> g.length }

        memory.indices.map { i =>
          memory(i) + banksToUpdate.getOrElse(i, 0)
        }.toVector
      }

      val startIndex =
        memory.zipWithIndex.fold((0, 0)) { case ((accMax, accMaxIndex), (current, currentIndex)) =>
          if (current > accMax) (current, currentIndex)
          else (accMax, accMaxIndex)
        }._2

      reallocate(memory.updated(startIndex, 0), memory(startIndex), startIndex)
    }

    def reallocateUntilLoop(memory: Memory, history: Seq[Int], currentCycle: Int): (Int, Int) = {
      val newState = reallocate(memory)
      val newStateFootprint = newState.hashCode()

      val newStateHistoryIndex = history.indexOf(newStateFootprint)

      if (newStateHistoryIndex >= 0) (currentCycle, history.size - newStateHistoryIndex)
      else reallocateUntilLoop(newState, history :+ newStateFootprint, currentCycle + 1)
    }

    reallocateUntilLoop(memory, Seq.empty, 1)
  }

  println(s"solve(...) = ${solve(Vector(0, 2, 7, 0))}")
}
