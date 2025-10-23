package example

import example.util.FastScanner

import io.github.acl4s.McfGraph
import io.github.acl4s.McfGraph.Edge
import io.github.acl4s.internal.foreach

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_e]]
 */
object E_MinCostFlow {

  private val Base: Long = 1L << 40

  def main(args: Array[String]): Unit = {
    val in = new FastScanner(System.in)
    val out = new java.io.PrintWriter(System.out)

    val n = in.nextInt()
    val k = in.nextLong()
    val grid = Array.fill(n, n)(in.nextInt())

    val s = n * 2
    val t = s + 1
    val g = McfGraph(n * 2 + 2)

    for {
      row <- 0 until n
      col <- 0 until n
    } {
      val cost = Base - grid(row)(col).toLong
      g.addEdge(row, col + n, 1L, cost)
    }

    foreach(0 until n)(i => {
      g.addEdge(s, i, k, 0L)
      g.addEdge(i + n, t, k, 0L)
    })
    g.addEdge(s, t, n * k, Base)

    val (_, minCost) = g.flow(s, t, n * k)
    val selected =
      g.edges
        .filterNot { case Edge(from, to, _, flow, _) => from == s || to == t || flow == 0L }
        .map(e => (e.from, e.to - n))
        .toSet

    out.println(Base * n * k - minCost)
    foreach(0 until n)(row => {
      val s = (0 until n)
        .map(col =>
          if (selected((row, col))) { 'X' }
          else { '.' }
        )
        .mkString
      out.println(s)
    })
    out.flush()
  }

}
