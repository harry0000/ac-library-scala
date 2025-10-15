package io.github.acl4s

import io.github.acl4s.internal.IPair

import scala.collection.mutable
import scala.util.boundary, boundary.break

final class MfGraph(private val n: Int) {
  import MfGraph.*

  private val pos: mutable.Buffer[IPair] = mutable.ArrayBuffer.empty
  private val g: Array[mutable.Buffer[_Edge]] = Array.fill(n)(mutable.ArrayBuffer.empty)

  def addEdge(from: Int, to: Int, cap: Long): Int = {
    assert(from < n)
    assert(to < n)
    assert(0L <= cap)

    val m = pos.size
    pos.addOne(IPair(from, g(from).size))

    val fromId = g(from).size
    val toId = g(to).size + (if (from == to) { 1 }
                             else { 0 })
    g(from).addOne(_Edge(to = to, rev = toId, cap))
    g(to).addOne(_Edge(to = from, rev = fromId, cap = 0L))

    m
  }

  def getEdge(i: Int): Edge = {
    assert(0 <= i && i < pos.size)

    val IPair(_1, _2) = pos(i)
    val _e = g(_1)(_2)
    val _re = g(_e.to)(_e.rev)
    Edge(from = _1, to = _e.to, cap = _e.cap + _re.cap, flow = _re.cap)
  }

  def edges(): Seq[Edge] = pos.indices.map(getEdge)

  def changeEdge(i: Int, newCap: Long, newFlow: Long): Unit = {
    assert(0 <= i && i < pos.size)
    assert(0L <= newFlow && newFlow <= newCap)

    val IPair(_1, _2) = pos(i)
    val _e = g(_1)(_2)
    val _re = g(_e.to)(_e.rev)
    _e.cap = newCap - newFlow
    _re.cap = newFlow
  }

  def flow(s: Int, t: Int, flowLimit: Long = Long.MaxValue): Long = {
    assert(0 <= s && s < n)
    assert(0 <= t && t < n)
    assert(s != t)

    val level: Array[Int] = new Array(n)
    val iter: Array[Int] = new Array(n)
    val queue = mutable.Queue.empty[Int]

    def bfs(): Unit = {
      java.util.Arrays.fill(level, -1)
      level(s) = 0
      queue.clear()
      queue.enqueue(s)

      boundary {
        while (queue.nonEmpty) {
          val v = queue.dequeue()
          g(v).foreach(e => {
            // if (e.cap == 0 || level[e.to] >= 0) continue;
            if (e.cap != 0 && level(e.to) < 0) {
              level(e.to) = level(v) + 1
              // if (e.to == t) return;
              if (e.to == t) { break(()) }
              queue.enqueue(e.to)
            }
          })
        }
      }
    }

    def dfs(v: Int, up: Long): Long = {
      if (v == s) { return up }
      var res = 0L
      val levelV = level(v)
      var i = iter(v)
      while (i < g(v).size) {
        val e = g(v)(i)
        val re = g(e.to)(e.rev)
        // if (level_v <= level[e.to] || g[e.to][e.rev].cap == 0) continue;
        if (levelV > level(e.to) && re.cap != 0) {
          val d = dfs(e.to, (up - res).min(re.cap))
          // if (d <= 0) continue;
          if (d > 0) {
            e.cap += d
            re.cap -= d
            res += d
            if (res == up) { return res }
          }
        }
        i += 1
      }
      level(v) = n
      res
    }

    var flow = 0L
    while (flow < flowLimit) {
      bfs()
      // if (level[t] == -1) break;
      if (level(t) < 0) { return flow }
      java.util.Arrays.fill(iter, 0)
      val f = dfs(t, flowLimit - flow)
      // if (!f) break;
      if (f == 0) { return flow }
      flow += f
    }
    flow
  }

  def minCut(s: Int): collection.IndexedSeq[Boolean] = {
    val visited = mutable.ArrayBuffer.fill(n)(false)
    val queue = mutable.Queue.empty[Int]
    visited(s) = true
    queue.enqueue(s)
    while (queue.nonEmpty) {
      val p = queue.dequeue()
      g(p).foreach(e => {
        if (e.cap > 0L && !visited(e.to)) {
          visited(e.to) = true
          queue.enqueue(e.to)
        }
      })
    }
    visited
  }
}

object MfGraph {
  final case class Edge(from: Int, to: Int, cap: Long, flow: Long)
  final private case class _Edge(to: Int, rev: Int, var cap: Long)
}
