package io.github.acl4s.internal

class BitSuite extends munit.FunSuite {

  /**
   * https://github.com/atcoder/ac-library/blob/2088c8e2431c3f4d29a2cfabc6529fe0a0586c48/test/unittest/bit_test.cpp
   */
  test("ceilPow2()") {
    assertEquals(ceilPow2(0), 0)
    assertEquals(ceilPow2(1), 0)
    assertEquals(ceilPow2(2), 1)
    assertEquals(ceilPow2(3), 2)
    assertEquals(ceilPow2(4), 2)
    assertEquals(ceilPow2(5), 3)
    assertEquals(ceilPow2(6), 3)
    assertEquals(ceilPow2(7), 3)
    assertEquals(ceilPow2(8), 3)
    assertEquals(ceilPow2(9), 4)
    assertEquals(ceilPow2(1 << 30), 30)
    assertEquals(ceilPow2((1 << 30) + 1), 31)

    // There are no unsigned numeric primitive data types in Java.
    // assertEquals(ceilPow2(Int.MaxValue), 32)
  }

}
