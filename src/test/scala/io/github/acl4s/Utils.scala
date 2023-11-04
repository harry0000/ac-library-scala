package io.github.acl4s

import org.scalatest.diagrams.Diagrams
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec
import scala.util.Random

trait BaseSuite extends AnyFunSuite with Diagrams

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
