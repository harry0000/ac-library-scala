package example

import example.util.FastScanner

import io.github.acl4s.{convolution, ModInt998244353 as Mint}
import io.github.acl4s.given

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_f]]
 */
object F_Convolution {

  def main(args: Array[String]): Unit = {
    val in = FastScanner(System.in)
    val out = new java.io.PrintWriter(System.out)

    val n = in.nextInt()
    val m = in.nextInt()
    val a = Array.fill(n)(Mint(in.nextInt()))
    val b = Array.fill(m)(Mint(in.nextInt()))

    val ans = convolution(a, b)
    out.println(ans.map(_.value).mkString(" "))
    out.flush()
  }

}
