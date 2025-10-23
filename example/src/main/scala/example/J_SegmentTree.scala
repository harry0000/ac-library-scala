package example

import example.util.FastScanner

import io.github.acl4s.{Monoid, Segtree}
import io.github.acl4s.internal.foreach

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_j]]
 */
object J_SegmentTree {

  def main(args: Array[String]): Unit = {
    val in = FastScanner(System.in)
    val out = new java.io.PrintWriter(System.out)

    given Monoid[Int] with {
      def e(): Int = -1
      def combine(a: Int, b: Int): Int = a.max(b)
    }

    val n = in.nextInt()
    val q = in.nextInt()
    val segtree = Segtree(Array.fill(n)(in.nextInt()))

    foreach(0 until q)(_ => {
      val t = in.nextInt()
      t match {
        case 1 =>
          val x = in.nextInt()
          val v = in.nextInt()

          segtree.set(x-1, v)
        case 2 =>
          val l = in.nextInt()
          val r = in.nextInt()

          out.println(segtree.prod(l-1, r))
        case 3 =>
          val x = in.nextInt()
          val v = in.nextInt()

          out.println(segtree.maxRight(x - 1, _ < v) + 1)
        case _ =>
      }
    })
    out.flush()
  }

}
