package example

import example.util.FastScanner

import io.github.acl4s.FenwickTree
import io.github.acl4s.internal.foreach

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_b]]
 */
object B_FenwickTree {

  def main(args: Array[String]): Unit = {
    val in = new FastScanner(System.in)
    val out = new java.io.PrintWriter(System.out)

    val n = in.nextInt()
    val q = in.nextInt()
    val ft = FenwickTree(Array.fill(n)(in.nextLong()))

    foreach(0 until q)(_ => {
      val query = in.nextInt()

      if (query == 0) {
        val p = in.nextInt()
        val x = in.nextLong()

        ft.add(p, x)
      } else {
        val l = in.nextInt()
        val r = in.nextInt()

        out.println(ft.sum(l, r))
      }
    })
    out.flush()
  }

}
