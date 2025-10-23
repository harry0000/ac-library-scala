package io.github.acl4s

class SegtreeSuite extends munit.FunSuite {

  /**
   * @see https://onlinejudge.u-aizu.ac.jp/courses/library/3/DSL/2/DSL_2_A
   */
  test("Aizu Online Judge DSL_2_A Range Minimum Query (RMQ)") {
    given rmq: Monoid[Int] with {
      final override def e(): Int = Int.MaxValue
      final override def combine(a: Int, b: Int): Int = a.min(b)
    }

    val segtree = Segtree(3)
    segtree.set(0, 1)
    segtree.set(1, 2)
    segtree.set(2, 3)

    assertEquals(segtree.prod(0 until 2), 1)
    assertEquals(segtree.prod(1 until 2), 2)
    assertEquals(segtree.prod(0 until 3), 1)
    assertEquals(segtree.allProd(), 1)
  }

  /**
   * @see https://onlinejudge.u-aizu.ac.jp/courses/library/3/DSL/2/DSL_2_B
   */
  test("Aizu Online Judge DSL_2_B Range Sum Query") {
    given rsq: Monoid[Int] with {
      final override def e(): Int = 0
      final override def combine(a: Int, b: Int): Int = a + b
    }

    val segtree = Segtree(3)
    segtree.set(0, 1)
    segtree.set(1, 2)
    segtree.set(2, 3)

    assertEquals(segtree.prod(0, 2), 3)
    assertEquals(segtree.prod(1, 2), 2)
    assertEquals(segtree.prod(0, 3), 6)
    assertEquals(segtree.allProd(), 6)
  }

}
