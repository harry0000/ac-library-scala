package io.github.acl4s

import scala.annotation.tailrec
import scala.util.Random

@tailrec
def gcd(a: Long, b: Long): Long = {
  assert(0 <= a && 0 <= b)
  if (b == 0) {
    a
  } else {
    gcd(b, a % b)
  }
}

def randomInt(min: Int, max: Int): Int =
  Random.between(min, max + 1)

def randomBoolean(): Boolean =
  randomInt(0, 1) == 0
