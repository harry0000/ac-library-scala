package example

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_f]]
 */
class F_ConvolutionSuite extends BaseFunSuite {

  test("Sample Input 1") {
    val input =
      """4 5
        |1 2 3 4
        |5 6 7 8 9
        |""".stripMargin

    val output =
      """5 16 34 60 70 70 59 36""".stripMargin

    assertEquals(run(F_Convolution.main, input).lines().toList, output.lines().toList)
  }

  test("Sample Input 2") {
    val input =
      """1 1
        |10000000
        |10000000
        |""".stripMargin

    val output =
      """871938225""".stripMargin

    assertEquals(run(F_Convolution.main, input).lines().toList, output.lines().toList)
  }

}
