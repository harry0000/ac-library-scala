package io.github.acl4s.internal

import scala.reflect.ClassTag

private[acl4s] case class Csr[E] private (start: Array[Int], eList: Array[E])
private[acl4s] object Csr {
  def apply[E: ClassTag](n: Int, edges: collection.Seq[(Int, E)]): Csr[E] = {
    val csr = Csr(new Array[Int](n + 1), new Array[E](edges.size))
    for ((from, _) <- edges) {
      csr.start(from + 1) += 1
    }
    (1 to n).foreach(i => {
      csr.start(i) += csr.start(i - 1)
    })
    val counter = csr.start.clone()
    for ((from, edge) <- edges) {
      csr.eList(counter(from)) = edge
      counter(from) += 1
    }
    csr
  }
}
