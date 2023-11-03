package io.github.acl4s

import scala.annotation.tailrec

@tailrec
def gcd(a: Long, b: Long): Long = {
  assert(0 <= a && 0 <= b)
  if (b == 0) {
    a
  } else {
    gcd(b, a % b)
  }
}
