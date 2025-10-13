package io.github.acl4s

import scala.util.boundary, boundary.break

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
    assert(0 <= i && i < n)
    assert(0 <= j && j < n)
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
    boundary {
      for (i <- 0 until n) {
        if (id(2 * i) == id(2 * i + 1)) { break(false) }
        answer(i) = id(2 * i) < id(2 * i + 1)
      }
      true
    }
  }
}
