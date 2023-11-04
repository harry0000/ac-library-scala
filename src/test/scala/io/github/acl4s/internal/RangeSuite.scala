package io.github.acl4s.internal

import org.scalatest.funsuite.AnyFunSuite

class RangeSuite extends AnyFunSuite {

  test("rightOpenInterval()") {
    assert(rightOpenInterval(0 to 10) === (0, 11))
    assert(rightOpenInterval((0 to 10).reverse) === (0, 11))
    assert(rightOpenInterval(10 to 0 by -1) === (0, 11))

    assert(rightOpenInterval(0 until 10) === (0, 10))
    assert(rightOpenInterval((0 until 10).reverse) === (0, 10))
    assert(rightOpenInterval(10 until 0 by -1) === (1, 11))

    assert(rightOpenInterval(0 to 0) === (0, 1))
    assert(rightOpenInterval((0 until 0).reverse) === (0, 0))
    assert(rightOpenInterval(0 until 0 by -1) === (0, 0))
  }

}
