package io.github.acl4s.internal

import scala.collection.mutable
import scala.reflect.ClassTag

private[internal] case class Edge(to: Int)

private[internal] case class Csr[E] private (start: Array[Int], eList: Array[E])
private[internal] object Csr {
  def apply[E: ClassTag](n: Int, edges: collection.Seq[(Int, E)]): Csr[E] = {
    val csr = Csr(new Array[Int](n + 1), new Array[E](edges.size))
    for ((from, _) <- edges) {
      csr.start(from + 1) += 1
    }
    (1 to n).foreach(i => {
      csr.start(i) += csr.start(i - 1)
    })
    val counter = csr.start.clone()
    for ((from, edge) <- edges) {
      csr.eList(counter(from)) = edge
      counter(from) += 1
    }
    csr
  }
}

/**
 * Reference:
 * R. Tarjan,
 * Depth-First Search and Linear Graph Algorithms
 */
case class SccGraph(private val n: Int) {
  private val edges: mutable.Buffer[(Int, Edge)] = mutable.ListBuffer.empty

  def numVertices: Int = n

  def addEdge(from: Int, to: Int): Unit = {
    edges += ((from, Edge(to)))
  }

  /**
   * @return pair of (# of scc, scc id)
   */
  def sccIds(): (Int, Array[Int]) = {
    val g = Csr(n, edges)
    var now_ord = 0
    var group_num = 0
    val visited = new mutable.Stack[Int](n)
    val ord = Array.fill(n)(-1)
    val low = new Array[Int](n)
    val ids = new Array[Int](n)

    def dfs(v: Int): Unit = {
      low(v) = now_ord
      ord(v) = now_ord
      now_ord += 1
      visited.push(v)
      (g.start(v) until g.start(v + 1)).foreach(i => {
        val to = g.eList(i).to
        if (ord(to) == -1) {
          dfs(to)
          low(v) = low(v).min(low(to))
        } else {
          low(v) = low(v).min(ord(to))
        }
      })
      if (low(v) == ord(v)) {
        while {
          // do
          val u = visited.pop()
          ord(u) = n
          ids(u) = group_num

          // while
          u != v
        } do {}
        group_num += 1
      }
    }

    (0 until n).foreach(i => {
      if (ord(i) == -1) { dfs(i) }
    })
    (0 until n).foreach(i => {
      ids(i) = group_num - 1 - ids(i)
    })

    (group_num, ids)
  }

  def scc(): collection.Seq[collection.Seq[Int]] = {
    val (group_nums, ids) = sccIds()
    val counts = new Array[Int](group_nums)
    ids.foreach(x => { counts(x) += 1 })
    val groups = new mutable.ArrayBuffer[mutable.Buffer[Int]](n)
    (0 until group_nums).foreach(i => {
      groups += new mutable.ArrayBuffer[Int](counts(i))
    })
    (0 until n).foreach(i => {
      groups(ids(i)) += i
    })

    groups
  }
}
