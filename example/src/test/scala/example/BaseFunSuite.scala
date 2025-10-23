package example

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}

trait BaseFunSuite extends munit.FunSuite {

  protected def run(main: Array[String] => Unit, stdin: String): String = {
    val in = new ByteArrayInputStream(stdin.getBytes("UTF-8"))
    val out = new ByteArrayOutputStream()

    // swap System.in/out
    val originalIn = System.in
    val originalOut = System.out
    System.setIn(in)
    System.setOut(new PrintStream(out))

    try {
      main(Array.empty)
      out.toString("UTF-8")
    } finally {
      System.setIn(originalIn)
      System.setOut(originalOut)
    }
  }

}
