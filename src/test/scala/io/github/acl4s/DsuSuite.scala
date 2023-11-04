package io.github.acl4s

class DsuSuite extends BaseSuite {

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_a
   */
  test("AtCoder Library Practice Contest A - Disjoint Set Union") {
    val uf = Dsu(4)

    assert(uf.same(0, 1) === false)

    uf.merge(0, 1)
    uf.merge(2, 3)

    assert(uf.same(0, 1) === true)
    assert(uf.same(1, 2) === false)
    assert(uf.groups() === Seq(Seq(0, 1), Seq(2, 3)))

    uf.merge(0, 2)

    assert(uf.same(1, 3) === true)
    assert(uf.groups() === Seq(Seq(0, 1, 2, 3)))
  }

  test("zero") {
    val uf = Dsu(0)

    assert(uf.groups() === Nil)
  }

  test("simple") {
    val uf = Dsu(2)

    assert(uf.same(0, 1) === false)

    val x = uf.merge(0, 1)
    assert(uf.leader(0) === x)
    assert(uf.leader(1) === x)
    assert(uf.same(0, 1) === true)
    assert(uf.size(0) === 2)
  }

  test("line") {
    val n = 500_000
    val uf = Dsu(n)

    (0 until n - 1).foreach(i => {
      uf.merge(i, i + 1)
    })

    assert(uf.size(0) === n)
    assert(uf.groups().size === 1)
  }

  test("line reverse") {
    val n = 500_000
    val uf = Dsu(n)

    (0 until n - 1).reverse.foreach(i => {
      uf.merge(i, i + 1)
    })

    assert(uf.size(0) === n)
    assert(uf.groups().size === 1)
  }

}
