package example

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_a]]
 */
class A_DisjointSetUnionSuite extends BaseFunSuite {

  test("Sample Input 1") {
    val input =
      """4 7
        |1 0 1
        |0 0 1
        |0 2 3
        |1 0 1
        |1 1 2
        |0 0 2
        |1 1 3
        |""".stripMargin

    val output =
      """0
        |1
        |0
        |1""".stripMargin

    assertEquals(run(A_DisjointSetUnion.main, input).lines().toList, output.lines().toList)
  }

}
