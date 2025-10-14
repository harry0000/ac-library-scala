package io.github.acl4s

import io.github.acl4s.internal.Csr

import scala.collection.mutable

final class McfGraph(
  private val n: Int
) {
  import McfGraph.*

  private val _edges: mutable.IndexedBuffer[Edge] = mutable.ArrayBuffer.empty

  def addEdge(from: Int, to: Int, cap: Long, cost: Long): Int = {
    assert(0 <= from && from < n)
    assert(0 <= to && to < n)
    assert(0L <= cap)
    assert(0L <= cost)

    val m = _edges.size
    _edges.addOne(Edge(from, to, cap, 0L, cost))
    m
  }

  def edges: collection.IndexedSeq[Edge] = _edges

  def getEdge(i: Int): Edge = {
    assert(0 <= i && i < _edges.size)
    _edges(i)
  }

  def flow(s: Int, t: Int, flowLimit: Long = Long.MaxValue): (Long, Long) = {
    slope(s, t, flowLimit).lastOption.getOrElse((0L, 0L))
  }

  def slope(s: Int, t: Int, flowLimit: Long = Long.MaxValue): collection.Seq[(Long, Long)] = {
    assert(0 <= s && s < n)
    assert(0 <= t && t < n)
    assert(s != t)

    val m = _edges.size
    val edgeIdx = new Array[Int](m)

    val g = {
      val degree = new Array[Int](n)
      val redgeIdx = new Array[Int](m)
      val elist = new mutable.ArrayBuffer[(Int, _Edge)](2 * m)

      (0 until m).foreach(i => {
        val e = _edges(i)
        edgeIdx(i) = degree(e.from)
        redgeIdx(i) = degree(e.to)
        degree(e.from) += 1
        degree(e.to) += 1
        elist.addOne((e.from, _Edge(e.to, -1, e.cap - e.flow, e.cost)))
        elist.addOne((e.to, _Edge(e.from, -1, e.flow, -e.cost)))
      })
      val _g = Csr(n, elist)
      (0 until m).foreach(i => {
        val e = _edges(i)
        edgeIdx(i) += _g.start(e.from)
        redgeIdx(i) += _g.start(e.to)
        _g.eList(edgeIdx(i)).rev = redgeIdx(i)
        _g.eList(redgeIdx(i)).rev = edgeIdx(i)
      })
      _g
    }

    val result = _slope(g, s, t, flowLimit)

    (0 until m).foreach(i => {
      val e = g.eList(edgeIdx(i))
      _edges(i).flow = _edges(i).cap - e.cap
    })

    result
  }

  final private case class DualDist(var dual: Long, var dist: Long)

  private def _slope(
    g: Csr[_Edge],
    s: Int,
    t: Int,
    flowLimit: Long
  ): collection.Seq[(Long, Long)] = {
    // variants (C = maxcost):
    // -(n-1)C <= dual[s] <= dual[i] <= dual[t] = 0
    // reduced cost (= e.cost + dual[e.from] - dual[e.to]) >= 0 for all edge

    // dual_dist[i] = (dual[i], dist[i])
    val dualDist = Array.fill(n)(DualDist(0L, 0L))
    val prevE = new Array[Int](n)
    val visited = new Array[Boolean](n)

    type Q = (Long, Int)
    given Ordering[Q] = Ordering.by[Q, Long](_._1).reverse

    val qMin = mutable.ArrayBuffer.empty[Int]
    val heap = mutable.PriorityQueue.empty[Q]

    def dualRef(): Boolean = {
      dualDist.foreach(_.dist = Long.MaxValue)
      dualDist(s).dist = 0L

      java.util.Arrays.fill(visited, false)

      qMin.clear()
      heap.clear()

      qMin.addOne(s)
      while (qMin.nonEmpty || heap.nonEmpty) {
        val v =
          qMin.lastOption match {
            case Some(x) =>
              qMin.dropRightInPlace(1)
              x
            case None =>
              heap.dequeue()._2
          }
        // if (vis[v]) continue;
        if (!visited(v)) {
          visited(v) = true
          // if (v == t) break;
          if (v != t) {
            // dist[v] = shortest(s, v) + dual[s] - dual[v]
            // dist[v] >= 0 (all reduced cost are positive)
            // dist[v] <= (n-1)C
            val DualDist(dualV, distV) = dualDist(v)
            for {
              i <- g.start(v).until(g.start(v + 1))
              e = g.eList(i)
              // if (!e.cap) continue;
              if e.cap > 0
            } {
              // |-dual[e.to] + dual[v]| <= (n-1)C
              // cost <= C - -(n-1)C + 0 = nC
              val cost = e.cost - dualDist(e.to).dual + dualV
              if (dualDist(e.to).dist - distV > cost) {
                val distTo = distV + cost
                dualDist(e.to).dist = distTo
                prevE(e.to) = e.rev
                if (distTo == distV) {
                  qMin.addOne(e.to)
                } else {
                  heap.enqueue((distTo, e.to))
                }
              }
            }
          }
        }
      }
      if (!visited(t)) {
        return false
      }

      for {
        v <- 0 until n
        // if (!vis[v]) continue;
        if visited(v)
      } {
        // dual[v] = dual[v] - dist[t] + dist[v]
        //         = dual[v] - (shortest(s, t) + dual[s] - dual[t]) + (shortest(s, v) + dual[s] - dual[v])
        //         = - shortest(s, t) + dual[t] + shortest(s, v)
        //         = shortest(s, v) - shortest(s, t) >= 0 - (n-1)C
        dualDist(v).dual -= dualDist(t).dist - dualDist(v).dist
      }
      true
    }

    var flow = 0L
    var cost = 0L
    var prevCostPerFlow = -1L
    val result = mutable.ArrayBuffer((0L, 0L))
    while (flow < flowLimit) {
      // if (!dual_ref()) break;
      if (!dualRef()) { return result }

      var c = flowLimit - flow
      Iterator
        .iterate(t)(v => g.eList(prevE(v)).to)
        .takeWhile(_ != s)
        .foreach(v => {
          c = c.min(g.eList(g.eList(prevE(v)).rev).cap)
        })
      Iterator
        .iterate(t)(v => g.eList(prevE(v)).to)
        .takeWhile(_ != s)
        .foreach(v => {
          val e = g.eList(prevE(v))
          e.cap += c
          g.eList(e.rev).cap -= c
        })
      val d = -dualDist(s).dual
      flow += c
      cost += c * d
      if (prevCostPerFlow == d) {
        result.dropRightInPlace(1)
      }
      result.addOne((flow, cost))
      prevCostPerFlow = d
    }

    result
  }
}

object McfGraph {
  final case class Edge(from: Int, to: Int, cap: Long, var flow: Long, cost: Long)
  final private[McfGraph] case class _Edge(to: Int, var rev: Int, var cap: Long, cost: Long)
}
