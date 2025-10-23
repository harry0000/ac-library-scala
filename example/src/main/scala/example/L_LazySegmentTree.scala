package example

import example.util.FastScanner

import io.github.acl4s.{LazySegtree, MapMonoid, Monoid}
import io.github.acl4s.internal.foreach

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_l]]
 */
object L_LazySegmentTree {

  class S(var zero: Int, var one: Int, var inv: Long) {
    def flip(): Unit = {
      val zero = this.zero
      this.zero = this.one
      this.one = zero
      this.inv = this.zero.toLong * this.one.toLong - this.inv
    }
  }
  object S {
    def apply(bit: Int): S = new S(1 - bit, bit, 0L)
  }

  given Monoid[S] with {
    def e(): S = new S(0, 0, 0L)
    def combine(a: S, b: S): S = {
      new S(
        a.zero + b.zero,
        a.one + b.one,
        a.inv + b.inv + a.one.toLong * b.zero.toLong
      )
    }
  }

  given MapMonoid[S, Boolean] with {
    def id(): Boolean = false
    def mapping(f: Boolean, s: S): S = {
      if (f) { s.flip() }
      s
    }
    def composition(a: Boolean, b: Boolean): Boolean = a ^ b
  }

  def main(args: Array[String]): Unit = {
    val in = FastScanner(System.in)
    val out = new java.io.PrintWriter(System.out)

    val n = in.nextInt()
    val q = in.nextInt()
    val a = Array.fill(n)(S(in.nextInt()))

    val segtree = LazySegtree(a)

    foreach(0 until q)(_ => {
      val t = in.nextInt()
      val l = in.nextInt() - 1
      val r = in.nextInt()

      if (t == 1) {
        segtree.applyRange(l, r, true)
      } else {
        out.println(segtree.prod(l, r).inv)
      }
    })
    out.flush()
  }

}
