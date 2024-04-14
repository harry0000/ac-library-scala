package io.github.acl4s

class StringSuite extends munit.FunSuite {

  test("zAlgorithm") {
    {
      val str = "abracadabra";
      val lcp = zAlgorithm(str)

      assertEquals(lcp.toSeq, Seq(11, 0, 0, 1, 0, 1, 0, 4, 0, 0, 1))
    }
    {
      val str = "ababababa"
      val lcp = zAlgorithm(str)

      assertEquals(lcp.toSeq, Seq(9, 0, 7, 0, 5, 0, 3, 0, 1))
    }
  }

}
