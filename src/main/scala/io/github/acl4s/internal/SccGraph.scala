package io.github.acl4s.internal

import scala.collection.mutable

private[internal] case class Edge(to: Int)

/**
 * Reference:
 * R. Tarjan,
 * Depth-First Search and Linear Graph Algorithms
 */
final class SccGraph(private val n: Int) {
  private val edges: mutable.Buffer[(Int, Edge)] = mutable.ListBuffer.empty

  def numVertices: Int = n

  def addEdge(from: Int, to: Int): Unit = {
    edges.addOne((from, Edge(to)))
  }

  /**
   * @return pair of (# of scc, scc id)
   */
  def sccIds(): (Int, Array[Int]) = {
    val g = Csr(n, edges)
    var nowOrd = 0
    var groupNum = 0
    val visited = new mutable.Stack[Int](n)
    val ord = Array.fill(n)(-1)
    val low = new Array[Int](n)
    val ids = new Array[Int](n)

    def dfs(v: Int): Unit = {
      low(v) = nowOrd
      ord(v) = nowOrd
      nowOrd += 1
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
          ids(u) = groupNum

          // while
          u != v
        } do {}
        groupNum += 1
      }
    }

    (0 until n).foreach(i => {
      if (ord(i) == -1) { dfs(i) }
    })
    (0 until n).foreach(i => {
      ids(i) = groupNum - 1 - ids(i)
    })

    (groupNum, ids)
  }

  def scc(): collection.Seq[collection.Seq[Int]] = {
    val (groupNums, ids) = sccIds()
    val counts = new Array[Int](groupNums)
    ids.foreach(x => { counts(x) += 1 })
    val groups = new mutable.ArrayBuffer[mutable.Buffer[Int]](n)
    (0 until groupNums).foreach(i => {
      groups += new mutable.ArrayBuffer[Int](counts(i))
    })
    (0 until n).foreach(i => {
      groups(ids(i)) += i
    })

    groups
  }
}
