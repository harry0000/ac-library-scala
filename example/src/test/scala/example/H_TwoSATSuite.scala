package example

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_h]]
 */
class H_TwoSATSuite extends BaseFunSuite {

  test("Sample Input 1") {
    val input =
      """3 2
        |1 4
        |2 5
        |0 6
        |""".stripMargin

    val output =
      """Yes
        |4
        |2
        |0""".stripMargin

    assertEquals(run(H_TwoSAT.main, input).lines().toList, output.lines().toList)
  }

  test("Sample Input 2") {
    val input =
      """3 3
        |1 4
        |2 5
        |0 6
        |""".stripMargin

    val output =
      """No""".stripMargin

    assertEquals(run(H_TwoSAT.main, input).lines().toList, output.lines().toList)
  }

}
