package example

import example.util.FastScanner

import io.github.acl4s.MfGraph
import io.github.acl4s.MfGraph.Edge
import io.github.acl4s.internal.foreach

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_d]]
 */
object D_Maxflow {

  def main(args: Array[String]): Unit = {
    val in = new FastScanner(System.in)
    val out = new java.io.PrintWriter(System.out)

    val n = in.nextInt()
    val m = in.nextInt()
    val grid = Array.fill(n)(in.next().toCharArray)

    val s = n * m
    val t = s + 1
    val g = MfGraph(n * m + 2)

    def toVertex(y: Int, x: Int): Int = y * m + x
    def toCell(v: Int): (Int, Int) = (v / m, v % m)

    for {
      i <- 0 until n
      j <- 0 until m
      if grid(i)(j) != '#'
    } {
      if ((i + j) % 2 == 0) {
        for {
          (di, dj) <- List((-1, 0), (1, 0), (0, -1), (0, 1))
          ni = i + di
          nj = j + dj
          if 0 <= ni && ni < n && 0 <= nj && nj < m && grid(ni)(nj) != '#'
        } {
          g.addEdge(toVertex(i, j), toVertex(ni, nj), 1L)
        }

        g.addEdge(s, toVertex(i, j), 1L)
      } else {
        g.addEdge(toVertex(i, j), t, 1L)
      }
    }

    val maxPair = g.flow(s, t)
    out.println(maxPair)
    val result = grid.clone()
    for {
      Edge(from, to, _, flow) <- g.edges()
      if from != s && to != t && flow > 0L
      (fy, fx) = toCell(from)
      (ty, tx) = toCell(to)
    } {
      if (fy == ty) {
        if (fx < tx) {
          result(fy)(fx) = '>'
          result(ty)(tx) = '<'
        } else {
          result(fy)(fx) = '<'
          result(ty)(tx) = '>'
        }
      } else {
        if (fy < ty) {
          result(fy)(fx) = 'v'
          result(ty)(tx) = '^'
        } else {
          result(fy)(fx) = '^'
          result(ty)(tx) = 'v'
        }
      }
    }
    foreach(0 until n)(y => { out.println(result(y).mkString) })
    out.flush()
  }

}
