package io.github.acl4s

class SccGraphSuite extends munit.FunSuite {

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_g
   */
  test("AtCoder Library Practice Contest G - SCC") {
    val graph = SccGraph(6)
    graph.addEdge(1, 4)
    graph.addEdge(5, 2)
    graph.addEdge(3, 0)
    graph.addEdge(5, 5)
    graph.addEdge(4, 1)
    graph.addEdge(0, 3)
    graph.addEdge(4, 2)

    val scc = graph.scc()
    assertEquals(scc.size, 4)
    assertEquals(scc.map(_.toSet).toSet, Set(Set(5), Set(1, 4), Set(2), Set(0, 3)))
  }

  test("empty") {
    assertEquals(SccGraph(0).scc(), Seq())
  }

  test("simple") {
    val graph = SccGraph(2)
    graph.addEdge(0, 1)
    graph.addEdge(1, 0)

    val scc = graph.scc()
    assertEquals(scc.size, 1)
  }

  test("self loop") {
    val graph = SccGraph(2)
    graph.addEdge(0, 0)
    graph.addEdge(0, 0)
    graph.addEdge(1, 1)

    val scc = graph.scc()
    assertEquals(scc.size, 2)
  }
}
