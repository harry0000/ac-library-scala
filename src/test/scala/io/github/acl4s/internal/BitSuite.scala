package io.github.acl4s.internal

import io.github.acl4s.BaseSuite

class BitSuite extends BaseSuite {

  /**
   * https://github.com/atcoder/ac-library/blob/2088c8e2431c3f4d29a2cfabc6529fe0a0586c48/test/unittest/bit_test.cpp
   */
  test("ceilPow2()") {
    assert(ceilPow2(0) === 0)
    assert(ceilPow2(1) === 0)
    assert(ceilPow2(2) === 1)
    assert(ceilPow2(3) === 2)
    assert(ceilPow2(4) === 2)
    assert(ceilPow2(5) === 3)
    assert(ceilPow2(6) === 3)
    assert(ceilPow2(7) === 3)
    assert(ceilPow2(8) === 3)
    assert(ceilPow2(9) === 4)
    assert(ceilPow2(1 << 30) === 30)
    assert(ceilPow2((1 << 30) + 1) === 31)

    // There are no unsigned numeric primitive data types in Java.
    // assert(ceilPow2(Int.MaxValue) === 32)
  }

}
