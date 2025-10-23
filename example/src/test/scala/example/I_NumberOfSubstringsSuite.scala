package example

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_i]]
 */
class I_NumberOfSubstringsSuite extends BaseFunSuite {

  test("Sample Input 1") {
    val input =
      """abcbcba
        |""".stripMargin

    val output =
      """21""".stripMargin

    assertEquals(run(I_NumberOfSubstrings.main, input).lines().toList, output.lines().toList)
  }

  test("Sample Input 2") {
    val input =
      """mississippi
        |""".stripMargin

    val output =
      """53""".stripMargin

    assertEquals(run(I_NumberOfSubstrings.main, input).lines().toList, output.lines().toList)
  }

  test("Sample Input 3") {
    val input =
      """ababacaca
        |""".stripMargin

    val output =
      """33""".stripMargin

    assertEquals(run(I_NumberOfSubstrings.main, input).lines().toList, output.lines().toList)
  }

  test("Sample Input 4") {
    val input =
      """aaaaa
        |""".stripMargin

    val output =
      """5""".stripMargin

    assertEquals(run(I_NumberOfSubstrings.main, input).lines().toList, output.lines().toList)
  }

}
