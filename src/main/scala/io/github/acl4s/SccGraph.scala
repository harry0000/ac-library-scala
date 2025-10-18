package io.github.acl4s

final class SccGraph private (private val internal: io.github.acl4s.internal.SccGraph) {

  def this(n: Int) = {
    this(io.github.acl4s.internal.SccGraph(n))
  }

  def addEdge(from: Int, to: Int): Unit = {
    val n = internal.numVertices
    require(0 <= from && from < n)
    require(0 <= to && to < n)
    internal.addEdge(from, to)
  }

  def scc(): collection.Seq[collection.Seq[Int]] = {
    internal.scc()
  }

}
