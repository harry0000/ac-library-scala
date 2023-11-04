package io.github.acl4s

class DsuSuite extends munit.FunSuite {

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_a
   */
  test("AtCoder Library Practice Contest A - Disjoint Set Union") {
    val uf = Dsu(4)

    assertEquals(uf.same(0, 1), false)

    uf.merge(0, 1)
    uf.merge(2, 3)

    assertEquals(uf.same(0, 1), true)
    assertEquals(uf.same(1, 2), false)
    assertEquals(uf.groups(), Seq(Seq(0, 1), Seq(2, 3)))

    uf.merge(0, 2)

    assertEquals(uf.same(1, 3), true)
    assertEquals(uf.groups(), Seq(Seq(0, 1, 2, 3)))
  }

  test("zero") {
    val uf = Dsu(0)

    assertEquals(uf.groups(), Seq())
  }

  test("simple") {
    val uf = Dsu(2)

    assertEquals(uf.same(0, 1), false)

    val x = uf.merge(0, 1)
    assertEquals(uf.leader(0), x)
    assertEquals(uf.leader(1), x)
    assertEquals(uf.same(0, 1), true)
    assertEquals(uf.size(0), 2)
  }

  test("line") {
    val n = 500_000
    val uf = Dsu(n)

    (0 until n - 1).foreach(i => {
      uf.merge(i, i + 1)
    })

    assertEquals(uf.size(0), n)
    assertEquals(uf.groups().size, 1)
  }

  test("line reverse") {
    val n = 500_000
    val uf = Dsu(n)

    (0 until n - 1).reverse.foreach(i => {
      uf.merge(i, i + 1)
    })

    assertEquals(uf.size(0), n)
    assertEquals(uf.groups().size, 1)
  }

}
