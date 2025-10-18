package io.github.acl4s

import io.github.acl4s.MfGraph.Edge

class MaxFlowSuite extends munit.FunSuite {

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_d
   */
  test("AtCoder Library Practice Contest D - Maxflow") {
    val n = 3
    val m = 3
    val grid = Array(
      "#..",
      "..#",
      "..."
    ).map(_.toCharArray)

    val s = n * m
    val t = s + 1
    val g = MfGraph(n * m + 2)

    def toVertex(i: Int, j: Int): Int = i * m + j

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

    assertEquals(g.flow(s, t), 3L)

    val expectedEdges = 3
    val actualEdges = g.edges().count { case Edge(from, to, _, flow) => from != s && to != t && flow > 0L }
    assertEquals(actualEdges, expectedEdges)
  }

  test("simple") {
    val g = MfGraph(4)

    assertEquals(g.addEdge(0, 1, 1L), 0)
    assertEquals(g.addEdge(0, 2, 1L), 1)
    assertEquals(g.addEdge(1, 3, 1L), 2)
    assertEquals(g.addEdge(2, 3, 1L), 3)
    assertEquals(g.addEdge(1, 2, 1L), 4)
    assertEquals(g.flow(0, 3), 2L)

    assertEquals(g.getEdge(0), Edge(0, 1, 1L, 1L))
    assertEquals(g.getEdge(1), Edge(0, 2, 1L, 1L))
    assertEquals(g.getEdge(2), Edge(1, 3, 1L, 1L))
    assertEquals(g.getEdge(3), Edge(2, 3, 1L, 1L))
    assertEquals(g.getEdge(4), Edge(1, 2, 1L, 0L))

    assertEquals(g.minCut(0), IndexedSeq(true, false, false, false))
  }

  test("not simple") {
    val g = MfGraph(2)

    assertEquals(g.addEdge(0, 1, 1L), 0)
    assertEquals(g.addEdge(0, 1, 2L), 1)
    assertEquals(g.addEdge(0, 1, 3L), 2)
    assertEquals(g.addEdge(0, 1, 4L), 3)
    assertEquals(g.addEdge(0, 1, 5L), 4)
    assertEquals(g.addEdge(0, 0, 6L), 5)
    assertEquals(g.addEdge(1, 1, 7L), 6)
    assertEquals(g.flow(0, 1), 15L)

    assertEquals(g.getEdge(0), Edge(0, 1, 1L, 1L))
    assertEquals(g.getEdge(1), Edge(0, 1, 2L, 2L))
    assertEquals(g.getEdge(2), Edge(0, 1, 3L, 3L))
    assertEquals(g.getEdge(3), Edge(0, 1, 4L, 4L))
    assertEquals(g.getEdge(4), Edge(0, 1, 5L, 5L))

    assertEquals(g.minCut(0), IndexedSeq(true, false))
  }

  test("cut") {
    val g = MfGraph(3)

    assertEquals(g.addEdge(0, 1, 2L), 0)
    assertEquals(g.addEdge(1, 2, 1L), 1)
    assertEquals(g.flow(0, 2), 1L)

    assertEquals(g.getEdge(0), Edge(0, 1, 2L, 1L))
    assertEquals(g.getEdge(1), Edge(1, 2, 1L, 1L))

    assertEquals(g.minCut(0), IndexedSeq(true, true, false))
  }

  test("twice") {
    val g = MfGraph(3)

    assertEquals(g.addEdge(0, 1, 1L), 0)
    assertEquals(g.addEdge(0, 2, 1L), 1)
    assertEquals(g.addEdge(1, 2, 1L), 2)

    assertEquals(g.flow(0, 2), 2L)

    assertEquals(g.getEdge(0), Edge(0, 1, 1L, 1L))
    assertEquals(g.getEdge(1), Edge(0, 2, 1L, 1L))
    assertEquals(g.getEdge(2), Edge(1, 2, 1L, 1L))

    g.changeEdge(0, 100L, 10L)
    assertEquals(g.getEdge(0), Edge(0, 1, 100L, 10L))

    assertEquals(g.flow(0, 2), 0L)
    assertEquals(g.flow(0, 1), 90L)

    assertEquals(g.getEdge(0), Edge(0, 1, 100L, 100L))
    assertEquals(g.getEdge(1), Edge(0, 2, 1L, 1L))
    assertEquals(g.getEdge(2), Edge(1, 2, 1L, 1L))

    assertEquals(g.flow(2, 0), 2L)

    assertEquals(g.getEdge(0), Edge(0, 1, 100L, 99L))
    assertEquals(g.getEdge(1), Edge(0, 2, 1L, 0L))
    assertEquals(g.getEdge(2), Edge(1, 2, 1L, 0L))
  }

  test("bound") {
    val INF = Long.MaxValue
    val g = MfGraph(3)

    assertEquals(g.addEdge(0, 1, INF), 0)
    assertEquals(g.addEdge(1, 0, INF), 1)
    assertEquals(g.addEdge(0, 2, INF), 2)

    assertEquals(g.flow(0, 2), INF)

    assertEquals(g.getEdge(0), Edge(0, 1, INF, 0L))
    assertEquals(g.getEdge(1), Edge(1, 0, INF, 0L))
    assertEquals(g.getEdge(2), Edge(0, 2, INF, INF))
  }

  /**
   * @see https://github.com/atcoder/ac-library/issues/1
   */
  test("self loop") {
    val g = MfGraph(3)

    assertEquals(g.addEdge(0, 0, 100L), 0)

    assertEquals(g.getEdge(0), Edge(0, 0, 100L, 0L))
  }

  /**
   * @see https://github.com/atcoder/ac-library/issues/5
   */
  test("invalid") {
    val g = MfGraph(2)

    interceptMessage[IllegalArgumentException]("requirement failed") { g.flow(0, 0) }
    interceptMessage[IllegalArgumentException]("requirement failed") { g.flow(0, 0, 0L) }
  }

  test("stress") {
    for (_ <- 0 until 10_000) {
      val n = randomInt(2, 20)
      val m = randomInt(1, 100)

      val (s, t) = {
        val s = randomInt(0, n - 2)
        val t = randomInt(s + 1, n - 1)

        if (randomBoolean()) { (t, s) }
        else { (s, t) }
      }

      val g = MfGraph(n)
      (0 until m).foreach(_ => {
        val u = randomInt(0, n - 1)
        val v = randomInt(0, n - 1)
        val c = randomInt(0, 10_000)
        g.addEdge(u, v, c.toLong)
      })
      val flow = g.flow(s, t)
      var dual = 0L
      val cut = g.minCut(s)
      val vFlow = new Array[Long](n)
      g.edges()
        .foreach(e => {
          vFlow(e.from) -= e.flow
          vFlow(e.to) += e.flow
          if (cut(e.from) && !cut(e.to)) {
            dual += e.cap
          }
        })

      assertEquals(flow, dual)
      assertEquals(-flow, vFlow(s))
      assertEquals(flow, vFlow(t))
      for {
        i <- 0 until n
        if i != s && i != t
      } {
        assertEquals(vFlow(i), 0L)
      }
    }
  }

}
