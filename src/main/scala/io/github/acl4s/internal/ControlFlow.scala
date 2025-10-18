package io.github.acl4s.internal

inline def foreach(range: Range)(inline f: Int => Unit): Unit = {
  var i = range.start
  while (range.contains(i)) {
    f(i)
    i += range.step
  }
}
