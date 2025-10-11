package io.github.acl4s

final class SccGraph(private val internal: io.github.acl4s.internal.SccGraph) {

  def addEdge(from: Int, to: Int): Unit = {
    val n = internal.numVertices
    assert(0 <= from && from < n)
    assert(0 <= to && to < n)
    internal.addEdge(from, to)
  }

  def scc(): collection.Seq[collection.Seq[Int]] = {
    internal.scc()
  }

}

object SccGraph {
  def apply(n: Int): SccGraph = {
    new SccGraph(internal.SccGraph(n))
  }
}
