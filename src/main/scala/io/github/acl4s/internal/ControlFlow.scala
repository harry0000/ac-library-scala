package io.github.acl4s.internal

inline def foreach(range: Range)(inline f: Int => Unit): Unit = {
  if (range.nonEmpty) {
    var i = range.start
    val last = range.last
    val step = range.step
    while ({ // do
      f(i)
      i != last
    }) { // while
      i += step
    }
  }
}
