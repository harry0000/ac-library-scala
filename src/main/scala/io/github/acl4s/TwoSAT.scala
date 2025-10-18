package io.github.acl4s

/**
 * Reference:
 * B. Aspvall, M. Plass, and R. Tarjan,
 * A Linear-Time Algorithm for Testing the Truth of Certain Quantified Boolean Formulas
 *
 * @param n
 */
final class TwoSAT(private val n: Int) {
  val answer = new Array[Boolean](n)
  private val scc = io.github.acl4s.internal.SccGraph(2 * n)

  def addClause(i: Int, f: Boolean, j: Int, g: Boolean): Unit = {
    require(0 <= i && i < n)
    require(0 <= j && j < n)
    scc.addEdge(2 * i + (if (f) { 0 }
                         else { 1 }),
                2 * j + (if (g) { 1 }
                         else { 0 })
    )
    scc.addEdge(2 * j + (if (g) { 0 }
                         else { 1 }),
                2 * i + (if (f) { 1 }
                         else { 0 })
    )
  }

  def satisfiable(): Boolean = {
    val (_, id) = scc.sccIds()
    var i = 0
    while (i < n) {
      if (id(2 * i) == id(2 * i + 1)) { return false }
      answer(i) = id(2 * i) < id(2 * i + 1)
      i += 1
    }
    true
  }
}
