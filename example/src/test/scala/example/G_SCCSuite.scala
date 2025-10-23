package example

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_g]]
 */
class G_SCCSuite extends BaseFunSuite {

  test("Sample Input 1") {
    val input =
      """6 7
        |1 4
        |5 2
        |3 0
        |5 5
        |4 1
        |0 3
        |4 2
        |""".stripMargin

    val output =
      """4
        |1 5
        |2 1 4
        |1 2
        |2 0 3""".stripMargin

    assertEquals(run(G_SCC.main, input).lines().toList, output.lines().toList)
  }

}
