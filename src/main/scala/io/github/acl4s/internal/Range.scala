package io.github.acl4s.internal

private[acl4s] def rightOpenInterval(range: Range): (Int, Int) = {
  assert(range.step == 1 || range.step == -1)
  if (range.isEmpty) {
    (range.start, range.start)
  } else if (range.step > 0) {
    (range.start, range.last + 1)
  } else {
    (range.last, range.start + 1)
  }
}
