package example

import example.util.FastScanner

import io.github.acl4s.Dsu
import io.github.acl4s.internal.foreach

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_a]]
 */
object A_DisjointSetUnion {

  def main(args: Array[String]): Unit = {
    val in = new FastScanner(System.in)
    val out = new java.io.PrintWriter(System.out)

    val n = in.nextInt()
    val q = in.nextInt()
    val dsu = Dsu(n)

    foreach(0 until q)(_ => {
      val t = in.nextInt()
      val v = in.nextInt()
      val u = in.nextInt()

      if (t == 0) {
        dsu.merge(v, u)
      } else {
        out.println(if (dsu.same(v, u)) { 1 }
        else { 0 })
      }
    })
    out.flush()
  }

}
