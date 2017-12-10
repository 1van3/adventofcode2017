package aoc

object Day3 extends App {
  def solve1(n: Int): Int = {
    val SquareSize = 8

    case class Square(num: Int, minEl: Int, maxEl: Int)

    def findSquare(forElement: Int): Square = {
      def find(current: Square): Square = {
        if (forElement >= current.minEl && forElement <= current.maxEl) current
        else {
          val nextNum = current.num + 1
          val nextMin = current.maxEl + 1
          val nextSize = nextNum * SquareSize
          find(Square(nextNum, nextMin, nextMin + nextSize - 1))
        }
      }

      find(Square(0, 1, 1))
    }

    def buildSquareDistances(minElement: Int, maxElement: Int, maxDist: Int) = {
      case class Element(value: Int, dist: Int, wasIncremented: Boolean)

      (minElement to maxElement).foldRight(Seq.empty[Element]) { case (current, elements) =>
        val (currentDist, wasIncremented) = {
          val (prevDist, prevWasIncremented) = elements.reverse.headOption match {
            case Some(p) => (p.dist, p.wasIncremented)
            case None => (maxDist - 1, true)
          }

          val shouldIncrement =
            if (prevWasIncremented) prevDist + 1 <= maxDist
            else prevDist == 0

          if (shouldIncrement) (prevDist + 1, true)
          else (prevDist - 1, false)
        }

        elements :+ Element(current, currentDist, wasIncremented)
      }
      .map { case el => (el.value, el.dist) }
    }

    if (n == 1) 0
    else {
      val square = findSquare(n)
      val distanceOnSquare = {
        val maxDist = (square.num * SquareSize) / 4 / 2
        buildSquareDistances(square.minEl, square.maxEl, maxDist)
          .collectFirst { case (e, d) if e == n => d }
          .get
      }

      square.num + distanceOnSquare
    }
  }

  def solve2(n: Int): Int = {
    case class Square(num: Int, elements: Seq[Int])

    val boardSize = 100
    val board = Array.ofDim[Int](boardSize, boardSize)

    def buildUntil(): Int = {
      def build(row: Int, col: Int): Int = {
        def left() = board(row)(col - 1)
        def right() = board(row)(col + 1)
        def top() = board(row - 1)(col)
        def bottom() = board(row + 1)(col)
        def topLeft() = board(row - 1)(col - 1)
        def topRight() = board(row - 1)(col + 1)
        def bottomLeft() = board(row + 1)(col - 1)
        def bottomRight() = board(row + 1)(col + 1)

        val nextValue = left() + right() + top() + bottom() + topLeft() + topRight() + bottomLeft() + bottomRight()

        if (nextValue > n) nextValue
        else {
          board(row)(col) = nextValue

          def isEmpty(v: Int) = v == 0

          def moveRight() = if (!isEmpty(top()) && isEmpty(right())) Some((row, col + 1)) else None
          def moveUp() = if (!isEmpty(left()) && isEmpty(top())) Some((row - 1, col)) else None
          def moveLeft() = if (!isEmpty(bottom()) && isEmpty(left())) Some(row, col - 1) else None
          def moveDown() = if ((!isEmpty(right()) || !isEmpty(topRight())) && isEmpty(bottom())) Some((row + 1, col)) else None

          val Some((nextRow, nextCol)) = moveRight() orElse moveUp() orElse moveLeft() orElse moveDown() orElse moveRight()
          build(nextRow, nextCol)
        }
      }

      val (startR, startC) = (boardSize / 2, boardSize / 2)
      board(startR)(startC) = 1

      build(startR, startC + 1)
    }

    buildUntil()
  }

  println(s"solve2(325489) = ${solve2(325489)}")
}
