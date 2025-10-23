package io.github.acl4s

class TwoSATSuite extends munit.FunSuite {

  test("zero") {
    val ts = TwoSAT(0)
    assertEquals(ts.satisfiable(), true)
    assertEquals(ts.answer.toSeq, Seq())
  }

  test("one") {
    {
      val ts = TwoSAT(1)
      ts.addClause(0, true, 0, true)
      ts.addClause(0, false, 0, false)
      assertEquals(ts.satisfiable(), false)
    }
    {
      val ts = TwoSAT(1)
      ts.addClause(0, true, 0, true)
      assertEquals(ts.satisfiable(), true)
      assertEquals(ts.answer.toSeq, Seq(true))
    }
    {
      val ts = TwoSAT(1)
      ts.addClause(0, false, 0, false)
      assertEquals(ts.satisfiable(), true)
      assertEquals(ts.answer.toSeq, Seq(false))
    }
  }

  test("stress ok") {
    (0 until 10_000).foreach(_ => {
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

      assertEquals(ts.satisfiable(), true)

      val actual = ts.answer
      (0 until m).foreach(i => {
        val x = xs(i)
        val y = ys(i)
        types(i) match {
          case 0 => assertEquals(actual(x) == expect(x) || actual(y) == expect(y), true)
          case 1 => assertEquals(actual(x) != expect(x) || actual(y) == expect(y), true)
          case _ => assertEquals(actual(x) == expect(x) || actual(y) != expect(y), true)
        }
      })
    })
  }

}
