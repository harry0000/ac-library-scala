package io.github.acl4s

import scala.reflect.ClassTag

/**
 * Reference:
 * D. Gusfield,
 * Algorithms on Strings, Trees, and Sequences: Computer Science and
 * Computational Biology
 */
private[acl4s] def zAlgorithmImpl[T: ClassTag](s: Array[T]): Array[Int] = {
  val n = s.length
  if (n == 0) {
    return Array.empty
  }
  val z = new Array[Int](n)
  z(0) = 0
  var j = 0
  for (i <- 1 until n) {
    var k = if (j + z(j) <= i) { 0 }
    else { Math.min(j + z(j) - i, z(i - j)) }
    while (i + k < n && s(k) == s(i + k)) {
      k += 1
    }
    z(i) = k
    if (j + z(j) < i + z(i)) {
      j = i
    }
  }
  z(0) = n
  z
}

def zAlgorithm(s: String): Array[Int] = zAlgorithmImpl(s.toCharArray)
