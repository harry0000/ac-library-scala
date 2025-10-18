package io.github.acl4s.internal

import scala.annotation.tailrec

@tailrec
private def foreachImpl(range: Range, i: Int, f: Int => Unit): Unit = {
  if (range.contains(i)) {
    f(i)
    foreachImpl(range, i + range.step, f)
  } else {
    ()
  }
}

inline def foreach(range: Range)(f: Int => Unit): Unit = {
  foreachImpl(range, range.start, f)
}
