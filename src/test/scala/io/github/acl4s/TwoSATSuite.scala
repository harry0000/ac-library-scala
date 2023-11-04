package io.github.acl4s

class TwoSATSuite extends BaseSuite {

  /**
   * @see https://atcoder.jp/contests/practice2/tasks/practice2_h
   */
  test("AtCoder Library Practice Contest H - Two SAT") {
    val n = 3
    val x = Array(1, 2, 0)
    val y = Array(4, 5, 6)

    {
      // Sample Input 1
      val d = 2

      val ts = TwoSAT(n)
      for {
        i <- 0 until n
        j <- i + 1 until n
      } {
        if ((x(i) - x(j)).abs < d) {
          ts.addClause(i, false, j, false)
        }
        if ((x(i) - y(j)).abs < d) {
          ts.addClause(i, false, j, true)
        }
        if ((y(i) - x(j)).abs < d) {
          ts.addClause(i, true, j, false)
        }
        if ((y(i) - y(j)).abs < d) {
          ts.addClause(i, true, j, true)
        }
      }

      assert(ts.satisfiable() === true)

      val res = ts.answer.zipWithIndex
        .map((v, i) =>
          if (v) { x(i) }
          else { y(i) }
        )
        .sorted
      val min_distance = (1 until n).map(i => res(i) - res(i - 1)).min
      assert(min_distance >= d)
    }

    {
      // Sample Input 2
      val d = 3

      val ts = TwoSAT(n)
      for {
        i <- 0 until n
        j <- i + 1 until n
      } {
        if ((x(i) - x(j)).abs < d) {
          ts.addClause(i, false, j, false)
        }
        if ((x(i) - y(j)).abs < d) {
          ts.addClause(i, false, j, true)
        }
        if ((y(i) - x(j)).abs < d) {
          ts.addClause(i, true, j, false)
        }
        if ((y(i) - y(j)).abs < d) {
          ts.addClause(i, true, j, true)
        }
      }

      assert(ts.satisfiable() === false)
    }
  }

  test("zero") {
    val ts = TwoSAT(0)
    assert(ts.satisfiable() === true)
    assert(ts.answer.toSeq === Nil)
  }

  test("one") {
    {
      val ts = TwoSAT(1)
      ts.addClause(0, true, 0, true)
      ts.addClause(0, false, 0, false)
      assert(ts.satisfiable() === false)
    }
    {
      val ts = TwoSAT(1)
      ts.addClause(0, true, 0, true)
      assert(ts.satisfiable() === true)
      assert(ts.answer.toSeq === Seq(true))
    }
    {
      val ts = TwoSAT(1)
      ts.addClause(0, false, 0, false)
      assert(ts.satisfiable() === true)
      assert(ts.answer.toSeq === Seq(false))
    }
  }

  test("stress ok") {
    (0 until 10_000).foreach(phase => {
      val n = randomInt(1, 20)
      val m = randomInt(1, 100)
      val expect = Array.fill(n)(randomBoolean())
      val ts = TwoSAT(n)

      val xs = new Array[Int](m)
      val ys = new Array[Int](m)
      val types = new Array[Int](m)
      (0 until m).foreach(i => {
        val x = randomInt(0, n - 1)
        val y = randomInt(0, n - 1)
        val ty = randomInt(0, 2)
        xs(i) = x
        ys(i) = y
        types(i) = ty
        ty match {
          case 0 => ts.addClause(x, expect(x), y, expect(y))
          case 1 => ts.addClause(x, !expect(x), y, expect(y))
          case _ => ts.addClause(x, expect(x), y, !expect(y))
        }
      })

      assert(ts.satisfiable() === true)

      val actual = ts.answer
      (0 until m).foreach(i => {
        val x = xs(i)
        val y = ys(i)
        types(i) match {
          case 0 => assert(actual(x) == expect(x) || actual(y) == expect(y) === true)
          case 1 => assert(actual(x) != expect(x) || actual(y) == expect(y) === true)
          case _ => assert(actual(x) == expect(x) || actual(y) != expect(y) === true)
        }
      })
    })
  }

}
