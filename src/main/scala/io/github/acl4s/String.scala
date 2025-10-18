package io.github.acl4s

import io.github.acl4s.internal.foreach

def suffixArray(s: Array[Int], upper: Int): Array[Int] = {
  require(0 <= upper)
  s.foreach(v => require(0 <= v && v <= upper))
  internal.saIs(s.clone(), upper)
}

def suffixArrayArbitrary[T](s: Array[T])(using Ordering[T]): Array[Int] = {
  import scala.math.Ordered.orderingToOrdered

  val n = s.length
  val idx = Array.tabulate(n)(identity).sortWith((l, r) => s(l) < s(r))
  val s2 = new Array[Int](n)
  var now = 0
  foreach(0 until n)(i => {
    if (i > 0 && s(idx(i - 1)) != s(idx(i))) {
      now += 1
    }
    s2(idx(i)) = now
  })
  internal.saIs(s2, now)
}

def suffixArray(s: String): Array[Int] = {
  val s2 = s.toCharArray.map(_.toInt)
  internal.saIs(s2, s2.maxOption.getOrElse(255))
}

/**
 * Reference:
 * T. Kasai, G. Lee, H. Arimura, S. Arikawa, and K. Park,
 * Linear-Time Longest-Common-Prefix Computation in Suffix Arrays and Its Applications
 */
def lcpArrayArbitrary[T](s: Array[T], sa: Array[Int]): Array[Int] = {
  require(s.length == sa.length)
  val n = s.length
  require(n >= 1)

  val rnk = new Array[Int](n)
  foreach(sa.indices)(i => {
    assert(0 <= sa(i) && sa(i) < n)
    rnk(sa(i)) = i
  })
  val lcp = new Array[Int](n - 1)
  var h = 0
  foreach(0 until n)(i => {
    if (h > 0) { h -= 1 }
    val r = rnk(i)
    // if (rnk[i] == 0) continue;
    if (r != 0) {
      val j = sa(r - 1)
      while (j + h < n && i + h < n && s(j + h) == s(i + h)) {
        h += 1
      }
      lcp(r - 1) = h
    }
  })
  lcp
}

def lcpArray(s: String, sa: Array[Int]): Array[Int] = {
  lcpArrayArbitrary(s.toCharArray.map(_.toInt), sa)
}

/**
 * Reference:
 * D. Gusfield,
 * Algorithms on Strings, Trees, and Sequences: Computer Science and
 * Computational Biology
 */
private def zAlgorithmImpl[T](s: Array[T]): Array[Int] = {
  val n = s.length
  if (n == 0) {
    return Array.empty
  }
  val z = new Array[Int](n)
  z(0) = 0
  var j = 0
  foreach(1 until n)(i => {
    var k =
      if (j + z(j) <= i) { 0 }
      else { Math.min(j + z(j) - i, z(i - j)) }
    while (i + k < n && s(k) == s(i + k)) {
      k += 1
    }
    z(i) = k
    if (j + z(j) < i + z(i)) {
      j = i
    }
  })
  z(0) = n
  z
}

def zAlgorithm(s: String): Array[Int] = zAlgorithmImpl(s.toCharArray)
