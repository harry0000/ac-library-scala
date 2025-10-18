package io.github.acl4s.internal

import scala.collection.mutable

private[acl4s] def saNaive(s: Array[Int]): Array[Int] = {
  val n = s.length
  val sa = Array.tabulate(n)(identity)
  def cmp(l: Int, r: Int): Boolean = {
    if (l == r) {
      false
    } else {
      var li = l
      var ri = r
      while (li < n && ri < n) {
        if (s(li) != s(ri)) { return s(li) < s(ri) }
        li += 1
        ri += 1
      }
      li == n
    }
  }
  sa.sortWith(cmp)
}

private[acl4s] def saDoubling(s: Array[Int]): Array[Int] = {
  val n = s.length
  var sa = Array.tabulate(n)(identity)
  var rnk = s.clone()
  var tmp = new Array[Int](n)
  var k = 1
  while (k < n) {
    def cmp(x: Int, y: Int): Boolean = {
      if (rnk(x) != rnk(y)) { return rnk(x) < rnk(y) }
      val rx = if (x + k < n) rnk(x + k) else -1
      val ry = if (y + k < n) rnk(y + k) else -1
      rx < ry
    }
    sa = sa.sortWith(cmp)
    tmp(sa(0)) = 0
    foreach(1 until n)(i => {
      tmp(sa(i)) = tmp(sa(i - 1)) + (if (cmp(sa(i - 1), sa(i))) 1 else 0)
    })
    // std::swap(tmp, rnk);
    val work = rnk
    rnk = tmp
    tmp = work
    k *= 2
  }
  sa
}

private val ThresholdNaive = 10
private val ThresholdDoubling = 40

/**
 * SA-IS, linear-time suffix array construction
 * Reference:
 * G. Nong, S. Zhang, and W. H. Chan,
 * Two Efficient Algorithms for Linear Time Suffix Array Construction
 */
private[acl4s] def saIs(s: Array[Int], upper: Int): Array[Int] = {
  val n = s.length
  if (n == 0) { return Array.empty[Int] }
  if (n == 1) { return Array(0) }
  if (n == 2) {
    if (s(0) < s(1)) {
      return Array(0, 1)
    } else {
      return Array(1, 0)
    }
  }
  if (n < ThresholdNaive) {
    return saNaive(s)
  }
  if (n < ThresholdDoubling) {
    return saDoubling(s)
  }

  val sa = new Array[Int](n)
  val ls = new Array[Boolean](n)
  foreach((n - 2) to 0 by -1)(i => {
    ls(i) = {
      if (s(i) == s(i + 1)) { ls(i + 1) }
      else { s(i) < s(i + 1) }
    }
  })

  val sumL = new Array[Int](upper + 1)
  val sumS = new Array[Int](upper + 1)
  foreach(0 until n)(i => {
    if (!ls(i)) {
      sumS(s(i)) += 1
    } else {
      sumL(s(i) + 1) += 1
    }
  })
  foreach(0 until upper)(i => {
    sumS(i) += sumL(i)
    sumL(i + 1) += sumS(i)
  })
  sumS(upper) += sumL(upper)

  def induce(lms: collection.IndexedSeq[Int]): Unit = {
    java.util.Arrays.fill(sa, -1)
    val buf = sumS.clone()
    foreach(lms.indices)(i => {
      val d = lms(i)
      if (d != n) {
        sa(buf(s(d))) = d
        buf(s(d)) += 1
      }
    })
    Array.copy(src = sumL, srcPos = 0, dest = buf, destPos = 0, length = upper + 1)
    sa(buf(s(n - 1))) = n - 1
    buf(s(n - 1)) += 1
    foreach(sa.indices)(i => {
      val v = sa(i)
      if (v >= 1 && !ls(v - 1)) {
        sa(buf(s(v - 1))) = v - 1
        buf(s(v - 1)) += 1
      }
    })
    Array.copy(src = sumL, srcPos = 0, dest = buf, destPos = 0, length = upper + 1)
    foreach((n - 1) to 0 by -1)(i => {
      val v = sa(i)
      if (v >= 1 && ls(v - 1)) {
        buf(s(v - 1) + 1) -= 1
        sa(buf(s(v - 1) + 1)) = v - 1
      }
    })
  }

  val lmsMap = Array.fill(n + 1)(-1)
  var m = 0
  foreach(1 until n)(i => {
    if (!ls(i - 1) && ls(i)) {
      lmsMap(i) = m
      m += 1
    }
  })
  val lms = new mutable.ArrayBuffer[Int](m)
  foreach(1 until n)(i => {
    if (!ls(i - 1) && ls(i)) {
      lms.addOne(i)
    }
  })

  induce(lms)

  if (m > 0) {
    val sortedLms = new mutable.ArrayBuffer[Int](m)
    sa.foreach(v => {
      if (lmsMap(v) != -1) { sortedLms.addOne(v) }
    })
    val recS = new Array[Int](m)
    var recUpper = 0
    // recS(lmsMap(sortedLms(0))) = 0
    foreach(1 until m)(i => {
      var l = sortedLms(i - 1)
      var r = sortedLms(i)
      val endL = if (lmsMap(l) + 1 < m) lms(lmsMap(l) + 1) else n
      val endR = if (lmsMap(r) + 1 < m) lms(lmsMap(r) + 1) else n
      var same = true
      if (endL - l != endR - r) {
        same = false
      } else {
        while (l < endL && s(l) == s(r)) {
          l += 1
          r += 1
        }
        if (l == n || s(l) != s(r)) {
          same = false
        }
      }
      if (!same) { recUpper += 1 }
      recS(lmsMap(sortedLms(i))) = recUpper
    })

    val recSa = saIs(recS, recUpper)
    foreach(0 until m)(i => {
      sortedLms(i) = lms(recSa(i))
    })
    induce(sortedLms)
  }
  sa
}
