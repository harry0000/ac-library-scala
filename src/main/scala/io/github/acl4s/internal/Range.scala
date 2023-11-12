package io.github.acl4s.internal

private[acl4s] def rightOpenInterval(range: Range): IPair = {
  assert(range.step == 1 || range.step == -1)
  if (range.isEmpty) {
    IPair(range.start, range.start)
  } else if (range.step > 0) {
    IPair(range.start, range.last + 1)
  } else {
    IPair(range.last, range.start + 1)
  }
}
