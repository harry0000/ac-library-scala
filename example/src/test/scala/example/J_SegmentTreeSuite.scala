package example

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_j]]
 */
class J_SegmentTreeSuite extends BaseFunSuite {

  test("Sample Input 1") {
    val input =
      """5 5
        |1 2 3 2 1
        |2 1 5
        |3 2 3
        |1 3 1
        |2 2 4
        |3 1 3
        |""".stripMargin

    val output =
      """3
        |3
        |2
        |6""".stripMargin

    assertEquals(run(J_SegmentTree.main, input).lines().toList, output.lines().toList)
  }

}
