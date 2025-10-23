package example

import example.util.FastScanner

import io.github.acl4s.floorSum
import io.github.acl4s.internal.foreach

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_c]]
 */
object C_FloorSum {

  def main(args: Array[String]): Unit = {
    val in = new FastScanner(System.in)
    val out = new java.io.PrintWriter(System.out)

    val t = in.nextInt()

    foreach(0 until t)(_ => {
      val n = in.nextLong()
      val m = in.nextLong()
      val a = in.nextLong()
      val b = in.nextLong()

      val ans = floorSum(n, m, a, b)
      out.println(ans)
    })
    out.flush()
  }

}
