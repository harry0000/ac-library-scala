package io.github.acl4s.internal

import io.github.acl4s.BaseSuite

class RangeSuite extends BaseSuite {

  test("rightOpenInterval()") {
    assert(rightOpenInterval(0 to 10) === IPair(0, 11))
    assert(rightOpenInterval((0 to 10).reverse) === IPair(0, 11))
    assert(rightOpenInterval(10 to 0 by -1) === IPair(0, 11))

    assert(rightOpenInterval(0 until 10) === IPair(0, 10))
    assert(rightOpenInterval((0 until 10).reverse) === IPair(0, 10))
    assert(rightOpenInterval(10 until 0 by -1) === IPair(1, 11))

    assert(rightOpenInterval(0 to 0) === IPair(0, 1))
    assert(rightOpenInterval((0 until 0).reverse) === IPair(0, 0))
    assert(rightOpenInterval(0 until 0 by -1) === IPair(0, 0))
  }

}
