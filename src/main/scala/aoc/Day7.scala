package aoc

import scala.io.Source

object Day7 extends App {
  type Node = (String, Int, Seq[String])

  val nodePattern = "^(\\pL+) \\((\\pN+)\\)( -> ([\\pL+, ]+))?".r("name", "weight", "_", "childs")

  def parse(line: String): Node =
    nodePattern.findAllMatchIn(line).map { m =>
      (
        m.group("name"),
        m.group("weight").toInt,
        Option(m.group("childs")).map { childs =>
          childs.split(",").map(_.trim).toSeq
        }.getOrElse(Seq.empty)
      )
    }.toSeq.head

  def findRoot(nodes: Seq[Node]) = {
    val nodesWithChilds = nodes.filter(_._3.nonEmpty)
    val allChilds = nodesWithChilds.flatMap(_._3).toSet

    nodesWithChilds.find { case (name, _, _) => !allChilds.contains(name) }.get
  }

  def solve1(nodes: Seq[Node]) = findRoot(nodes)._1

  def solve2(nodes: Seq[Node]) = {
    case class Leaf(name: String, weight: Int, childs: Seq[Leaf]) {
      def totalWeight(): Int = weight + childs.map(_.totalWeight()).sum
    }

    def buildTree(): Leaf = {
      val nodeMap = nodes.map(n => n._1 -> n).toMap

      def mkLeaf(node: Node): Leaf =
        Leaf(
          node._1,
          node._2,
          node._3.map(nodeName => mkLeaf(nodeMap(nodeName)))
        )

      mkLeaf(findRoot(nodes))
    }

    def balance(leaf: Leaf, weightToBalance: Int): Int = {
      val childWeights =
        leaf.childs
          .map { ch => ch -> ch.totalWeight() }

      val weightGroups =
        childWeights
          .groupBy(_._2)
          .map { case (weight, g) => weight -> g.map(_._1) }
          .toSeq

      if (weightGroups.length == 1) { // childs are balanced
        leaf.weight - weightToBalance
      } else {
        val (firstWeight, firstLeafs) = weightGroups.head
        val (secondWeight, secondLeafs) = weightGroups.last

        val unbalancedLeaf = if (firstLeafs.size == 1) firstLeafs.head else secondLeafs.head
        balance(unbalancedLeaf, Math.abs(firstWeight - secondWeight))
      }
    }

    balance(buildTree(), 0)
  }

  println(solve2(Source.fromFile("input_file_path").getLines.map(parse).toSeq))
}