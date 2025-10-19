package io.github.acl4s

class LazySegtreeSuite extends munit.FunSuite {

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_k
   */
  test("AtCoder Library Practice Contest K - Range Affine Range Sum") {
    val Mod = 998_244_353L

    final case class S(var sum: Int, var size: Int)
    object S {
      def apply(value: Int): S = new S(value, 1)
      def e(): S = S(0, 0)
    }

    given Monoid[S] with {
      final override def e(): S = S.e()
      final override def combine(a: S, b: S): S = S(
        ((a.sum.toLong + b.sum) % Mod).toInt,
        a.size + b.size
      )
    }

    final case class F(b: Int, c: Int)
    object F {
      def id(): F = F(1, 0)
    }

    given MapMonoid[S, F] with {
      final override def id(): F = F.id()
      final override def mapping(f: F, s: S): S = {
        val F(b, c) = f
        s.sum = ((s.sum.toLong * b + s.size.toLong * c) % Mod).toInt
        s
      }

      final override def composition(l: F, r: F): F = {
        val nb = (l.b.toLong * r.b) % Mod
        val nc = (l.b.toLong * r.c + l.c) % Mod
        F(nb.toInt, nc.toInt)
      }
    }

    val segtree = LazySegtree(
      Array(1, 2, 3, 4, 5).map(S(_))
    )

    assertEquals(segtree.prod(0, 5).sum, 15)

    segtree.applyRange(2, 4, F(100, 101))

    assertEquals(segtree.prod(0, 3).sum, 404)

    segtree.applyRange(1, 3, F(102, 103))

    assertEquals(segtree.prod(2, 5).sum, 41511)

    segtree.applyRange(2, 5, F(104, 105))

    assertEquals(segtree.prod(0, 5).sum, 4317767)
  }

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_l
   */
  test("AtCoder Library Practice Contest L - Lazy Segment Tree") {
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

    given monoid: Monoid[S] with {
      final override def e(): S = new S(0, 0, 0L)
      final override def combine(a: S, b: S): S = {
        new S(
          a.zero + b.zero,
          a.one + b.one,
          a.inv + b.inv + a.one.toLong * b.zero.toLong
        )
      }
    }

    given mapMonoid: MapMonoid[S, Boolean] with {
      final override def id(): Boolean = false
      final override def mapping(f: Boolean, s: S): S = {
        if (f) { s.flip() }
        s
      }
      final override def composition(a: Boolean, b: Boolean): Boolean = a ^ b
    }

    val segtree = LazySegtree(Array(0, 1, 0, 0, 1).map(S(_)))

    assertEquals(segtree.prod(0 until 5).inv, 2L)

    segtree.applyRange(2 until 4, true)

    assertEquals(segtree.prod(1, 5).inv, 0L)

    segtree.applyRange(0 until 3, true)

    assertEquals(segtree.prod(0 until 2).inv, 1L)
  }

  /**
   * @see https://atcoder.jp/contests/typical90/tasks/typical90_ac
   */
  test("Typical 90 029 - Long Bricks（★5）") {
    given rmq: Monoid[Int] with {
      final override def e(): Int = 0
      final override def combine(a: Int, b: Int): Int = a.max(b)
    }

    given ruq: MapMonoid[Int, Int] with {
      final override def id(): Int = 0
      final override def mapping(f: Int, s: Int): Int = f.max(s)
      final override def composition(a: Int, b: Int): Int = a.max(b)
    }

    {
      val blocks = Array((27, 100), (8, 39), (83, 97), (24, 75))
      val segtree = LazySegtree(100)

      val results = blocks.map((l, r) => {
        val height = segtree.prod(l - 1, r) + 1
        segtree.applyRange(l - 1, r, height)
        height
      })

      assertEquals(results.toSeq, Seq(1, 2, 2, 3))
    }

    {
      val blocks =
        Array((1, 500_000), (500_000, 500_000), (1, 500_000), (1, 1), (1, 500_000), (500_000, 500_000), (1, 500_000))
      val segtree = LazySegtree(500_000)

      val results = blocks.map((l, r) => {
        val height = segtree.prod(l - 1, r) + 1
        segtree.applyRange(l - 1, r, height)
        height
      })

      assertEquals(results.toSeq, Seq(1, 2, 3, 4, 5, 6, 7))
    }
  }

}
