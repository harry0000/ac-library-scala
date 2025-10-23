package io.github.acl4s

class SccGraphSuite extends munit.FunSuite {

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
