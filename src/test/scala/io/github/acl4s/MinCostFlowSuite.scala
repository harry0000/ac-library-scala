package io.github.acl4s

import io.github.acl4s.McfGraph.Edge

import scala.util.boundary, boundary.break

class MinCostFlowSuite extends munit.FunSuite {

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_e
   */
  test("AtCoder Library Practice Contest E - MinCostFlow") {
    val Base = 1L << 40

    def solve(n: Int, k: Long, grid: Array[Array[Int]]): (Long, collection.IndexedSeq[(Int, Int)]) = {
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

      for (i <- 0 until n) {
        g.addEdge(s, i, k, 0L)
        g.addEdge(i + n, t, k, 0L)
      }
      g.addEdge(s, t, n * k, Base)

      val (_, minCost) = g.flow(s, t, n * k)
      val selected =
        g.edges
          .filterNot { case Edge(from, to, _, flow, _) => from == s || to == t || flow == 0L }
          .map(e => (e.from, e.to - n))

      (Base * n * k - minCost, selected)
    }

    // Sample Input 1
    {
      val n = 3
      val k = 1L
      // format: off
      val grid = Array(
        Array(5, 3, 1),
        Array(1, 4, 8),
        Array(7, 6, 9),
      )
      // format: on

      val (actualCost, actualCells) = solve(n, k, grid)

      assertEquals(actualCost, 19L)
      assertEquals(actualCells, IndexedSeq((0, 0), (1, 2), (2, 1)))
    }
    // Sample Input 2
    {
      val n = 3
      val k = 2
      // format: off
      val grid = Array(
        Array(10, 10,  1),
        Array(10, 10,  1),
        Array( 1,  1, 10),
      )
      // format: on

      val (actualCost, actualCells) = solve(n, k, grid)

      assertEquals(actualCost, 50L)
      assertEquals(actualCells, IndexedSeq((0, 0), (0, 1), (1, 0), (1, 1), (2, 2)))
    }
  }

  test("simple") {
    val g = McfGraph(4)
    g.addEdge(0, 1, 1L, 1L)
    g.addEdge(0, 2, 1L, 1L)
    g.addEdge(1, 3, 1L, 1L)
    g.addEdge(2, 3, 1L, 1L)
    g.addEdge(1, 2, 1L, 1L)

    assertEquals(g.slope(0, 3, 10L), Seq((0L, 0L), (2L, 4L)))
    assertEquals(g.getEdge(0), Edge(0, 1, 1L, 1L, 1L))
    assertEquals(g.getEdge(1), Edge(0, 2, 1L, 1L, 1L))
    assertEquals(g.getEdge(2), Edge(1, 3, 1L, 1L, 1L))
    assertEquals(g.getEdge(3), Edge(2, 3, 1L, 1L, 1L))
    assertEquals(g.getEdge(4), Edge(1, 2, 1L, 0L, 1L))
  }

  test("usage") {
    {
      val g = McfGraph(2)
      g.addEdge(0, 1, 1L, 2L)

      assertEquals(g.flow(0, 1), (1L, 2L))
    }
    {
      val g = McfGraph(2)
      g.addEdge(0, 1, 1L, 2L)

      assertEquals(g.slope(0, 1), Seq((0L, 0L), (1L, 2L)))
    }
  }

  test("out of range") {
    val g = McfGraph(10)

    interceptMessage[AssertionError]("assertion failed") { g.slope(-1, 3) }
    interceptMessage[AssertionError]("assertion failed") { g.slope(3, 3) }
  }

  test("self loop") {
    val g = McfGraph(3)

    assertEquals(g.addEdge(0, 0, 100L, 123L), 0)
    assertEquals(g.getEdge(0), Edge(0, 0, 100L, 0L, 123L))
  }

  test("same cost paths") {
    val g = McfGraph(3)

    assertEquals(g.addEdge(0, 1, 1L, 1L), 0)
    assertEquals(g.addEdge(1, 2, 1L, 0L), 1)
    assertEquals(g.addEdge(0, 2, 2L, 1L), 2)

    assertEquals(g.slope(0, 2), Seq((0L, 0L), (3L, 3L)))
  }

  /**
   * @see https://github.com/atcoder/ac-library/issues/51
   */
  test("invalid") {
    val g = McfGraph(2)

    interceptMessage[AssertionError]("assertion failed") { g.addEdge(0, 0, -1L, 0L) }
    interceptMessage[AssertionError]("assertion failed") { g.addEdge(0, 0, -1L, 0L) }
  }

  test("stress") {
    for (_ <- 0 until 1_000) {
      val n = randomInt(2, 20)
      val m = randomInt(1, 100)
      val (s, t) = {
        val s = randomInt(0, n - 2)
        val t = randomInt(s + 1, n - 1)
        if (randomBoolean()) (s, t) else (t, s)
      }

      val gMf = MfGraph(n)
      val gMcf = McfGraph(n)
      for (_ <- 0 until m) {
        val u = randomInt(0, n - 1)
        val v = randomInt(0, n - 1)
        val cap = randomInt(1, 10).toLong
        val cost = randomInt(0, 10_000).toLong
        gMf.addEdge(u, v, cap)
        gMcf.addEdge(u, v, cap, cost)
      }
      val (flow, cost) = gMcf.flow(s, t)
      assertEquals(flow, gMf.flow(s, t))

      var expectedCost = 0L
      val vCap = new Array[Long](n)
      gMcf.edges.foreach(e => {
        vCap(e.from) -= e.flow
        vCap(e.to) += e.flow
        expectedCost += e.flow * e.cost
      })
      assertEquals(cost, expectedCost)

      for (i <- 0 until n) {
        if (i == s) {
          assertEquals(vCap(i), -flow)
        } else if (i == t) {
          assertEquals(vCap(i), flow)
        } else {
          assertEquals(vCap(i), 0L)
        }
      }

      // check: there is no negative-cycle
      val dist = new Array[Long](n)
      var updated = false
      boundary {
        for (_ <- 0 until n) {
          updated = false
          gMcf.edges.foreach(e => {
            if (e.flow < e.cap) {
              val nd = dist(e.from) + e.cost
              if (nd < dist(e.to)) {
                dist(e.to) = nd
                updated = true
              }
            }
            if (e.flow > 0L) {
              val nd = dist(e.to) - e.cost
              if (nd < dist(e.from)) {
                dist(e.from) = nd
                updated = true
              }
            }
          })
          if (!updated) { break() }
        }
      }
      val hasNegativeCycle = updated
      assertEquals(hasNegativeCycle, false)
    }
  }

}
