package example

import example.util.FastScanner

import io.github.acl4s.SccGraph
import io.github.acl4s.internal.foreach

/**
 * @see [[https://atcoder.jp/contests/practice2/tasks/practice2_g]]
 */
object G_SCC {

  def main(args: Array[String]): Unit = {
    // HACK: To avoid a StackOverflowError when running on a JVM
    val t = new Thread(null, new Solver, "solver", /* stackSize = */ 1L << 26)
    t.setUncaughtExceptionHandler((_, e) => e.printStackTrace(System.err))
    t.start()
    t.join()
  }

  class Solver extends Runnable {
    override def run(): Unit = {
      val in = FastScanner(System.in)
      val out = new java.io.PrintWriter(System.out)

      val n = in.nextInt()
      val m = in.nextInt()
      val g = SccGraph(n)
      foreach(0 until m)(_ => {
        val a = in.nextInt()
        val b = in.nextInt()

        g.addEdge(a, b)
      })

      val ans = g.scc()
      out.println(ans.size)
      for (vs <- ans) {
        out.print(s"${vs.size} ")
        out.println(vs.mkString(" "))
      }
      out.flush()
    }
  }

}
