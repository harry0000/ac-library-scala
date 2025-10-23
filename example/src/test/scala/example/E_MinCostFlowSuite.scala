package example

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_e]]
 */
class E_MinCostFlowSuite extends BaseFunSuite {

  test("Sample Input 1") {
    val input =
      """3 1
        |5 3 2
        |1 4 8
        |7 6 9
        |""".stripMargin

    val output =
      """19
        |X..
        |..X
        |.X.""".stripMargin

    assertEquals(run(E_MinCostFlow.main, input).lines().toList, output.lines().toList)
  }

  test("Sample Input 2") {
    val input =
      """3 2
        |10 10 1
        |10 10 1
        |1 1 10
        |""".stripMargin

    val output =
      """50
        |XX.
        |XX.
        |..X""".stripMargin

    assertEquals(run(E_MinCostFlow.main, input).lines().toList, output.lines().toList)
  }

}
