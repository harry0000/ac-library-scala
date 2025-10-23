package example

import example.util.FastScanner

import io.github.acl4s.TwoSAT
import io.github.acl4s.internal.foreach

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_h]]
 */
object H_TwoSAT {

  def main(args: Array[String]): Unit = {
    val in = FastScanner(System.in)
    val out = new java.io.PrintWriter(System.out)

    val n = in.nextInt()
    val d = in.nextInt()
    val x = new Array[Int](n)
    val y = new Array[Int](n)
    foreach(0 until n)(i => {
      x(i) = in.nextInt()
      y(i) = in.nextInt()
    })

    val ts = TwoSAT(n)
    foreach(0 until n)(i => {
      foreach((i + 1) until n)(j => {
        if ((x(i) - x(j)).abs < d) {
          ts.addClause(i, false, j, false)
        }
        if ((x(i) - y(j)).abs < d) {
          ts.addClause(i, false, j, true)
        }
        if ((y(i) - x(j)).abs < d) {
          ts.addClause(i, true, j, false)
        }
        if ((y(i) - y(j)).abs < d) {
          ts.addClause(i, true, j, true)
        }
      })
    })

    if (ts.satisfiable()) {
      out.println("Yes")
      foreach(0 until n)(i => {
        out.println(if (ts.answer(i)) { x(i) }
        else { y(i) })
      })
    } else {
      out.println("No")
    }
    out.flush()
  }

}
