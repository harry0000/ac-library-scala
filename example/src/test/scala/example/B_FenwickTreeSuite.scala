package example

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_b]]
 */
class B_FenwickTreeSuite extends BaseFunSuite {

  test("Sample Input 1") {
    val input =
      """5 5
        |1 2 3 4 5
        |1 0 5
        |1 2 4
        |0 3 10
        |1 0 5
        |1 0 3
        |""".stripMargin

    val output =
      """15
        |7
        |25
        |6""".stripMargin

    assertEquals(run(B_FenwickTree.main, input).lines().toList, output.lines().toList)
  }

}
