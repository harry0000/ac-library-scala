package io.github.acl4s

class LazySegtreeSuite extends munit.FunSuite {

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
