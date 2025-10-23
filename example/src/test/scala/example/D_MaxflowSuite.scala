package example

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_d]]
 */
class D_MaxflowSuite extends BaseFunSuite {

  test("Sample Input 1") {
    val input =
      """3 3
        |#..
        |..#
        |...
        |""".stripMargin

    val output =
      """3
        |#><
        |><#
        |><.""".stripMargin

    assertEquals(run(D_Maxflow.main, input).lines().toList, output.lines().toList)
  }

}
