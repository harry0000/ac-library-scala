package example

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_c]]
 */
class C_FloorSumSuite extends BaseFunSuite {

  test("Sample Input 1") {
    val input =
      """5
        |4 10 6 3
        |6 5 4 3
        |1 1 0 0
        |31415 92653 58979 32384
        |1000000000 1000000000 999999999 999999999
        |""".stripMargin

    val output =
      """3
        |13
        |0
        |314095480
        |499999999500000000""".stripMargin

    assertEquals(run(C_FloorSum.main, input).lines().toList, output.lines().toList)
  }

}
