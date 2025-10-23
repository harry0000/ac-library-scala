package example

import example.util.FastScanner

import io.github.acl4s.{lcpArray, suffixArray}

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_i]]
 */
object I_NumberOfSubstrings {

  def main(args: Array[String]): Unit = {
    val in = FastScanner(System.in)
    val out = new java.io.PrintWriter(System.out)

    val s = in.next()
    val n = s.length

    val sa = suffixArray(s)
    val lcp = lcpArray(s, sa)

    val total = n.toLong * (n + 1) / 2
    val sum = lcp.foldLeft(0L)(_ + _)

    out.println(total - sum)
    out.flush()
  }

}
