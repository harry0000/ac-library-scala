package io.github.acl4s.internal

class RangeSuite extends munit.FunSuite {

  test("rightOpenInterval()") {
    assertEquals(rightOpenInterval(0 to 10), IPair(0, 11))
    assertEquals(rightOpenInterval((0 to 10).reverse), IPair(0, 11))
    assertEquals(rightOpenInterval(10 to 0 by -1), IPair(0, 11))

    assertEquals(rightOpenInterval(0 until 10), IPair(0, 10))
    assertEquals(rightOpenInterval((0 until 10).reverse), IPair(0, 10))
    assertEquals(rightOpenInterval(10 until 0 by -1), IPair(1, 11))

    assertEquals(rightOpenInterval(0 to 0), IPair(0, 1))
    assertEquals(rightOpenInterval((0 until 0).reverse), IPair(0, 0))
    assertEquals(rightOpenInterval(0 until 0 by -1), IPair(0, 0))
  }

}
