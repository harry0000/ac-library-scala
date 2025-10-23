package example

import scala.annotation.targetName
import scala.language.implicitConversions

import example.util.FastScanner

import io.github.acl4s.{LazySegtree, MapMonoid, ModInt998244353 as Mint, Monoid}
import io.github.acl4s.given
import io.github.acl4s.internal.foreach

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_k]]
 */
object K_RangeAffineRangeSum {

  final case class S(var sum: Mint, var size: Int)
  object S {
    def apply(value: Int): S = new S(Mint(value), 1)
    def e(): S = S(Mint(0), 0)
  }

  given Monoid[S] with {
    def e(): S = S.e()
    def combine(a: S, b: S): S = S(
      a.sum + b.sum,
      a.size + b.size
    )
  }

  final case class F(b: Mint, c: Mint)
  object F {
    @targetName("applyFromInt")
    def apply(b: Int, c: Int): F = new F(Mint(b), Mint(c))
    def id(): F = F(Mint(1), Mint(0))
  }

  given MapMonoid[S, F] with {
    def id(): F = F.id()
    def mapping(f: F, s: S): S = {
      val F(b, c) = f
      s.sum = s.sum * b + c * s.size
      s
    }
    def composition(l: F, r: F): F = {
      val nb = l.b * r.b
      val nc = l.b * r.c + l.c
      F(nb, nc)
    }
  }

  def main(args: Array[String]): Unit = {
    val in = FastScanner(System.in)
    val out = new java.io.PrintWriter(System.out)

    val n = in.nextInt()
    val q = in.nextInt()

    val segtree = LazySegtree(Array.fill(n)(S(in.nextInt())))

    foreach(0 until q)(_ => {
      val query = in.nextInt()
      val l = in.nextInt()
      val r = in.nextInt()

      if (query == 0) {
        val b = in.nextInt()
        val c = in.nextInt()

        segtree.applyRange(l, r, F(b, c))
      } else {
        out.println(segtree.prod(l, r).sum.value)
      }
    })
    out.flush()
  }

}
