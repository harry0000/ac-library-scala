package io.github.acl4s

import scala.reflect.ClassTag

import io.github.acl4s.internal.{ceilPow2, rightOpenInterval}

final case class Segtree[T](
  n: Int,
  size: Int,
  log: Int,
  d: Array[T]
)(using m: Monoid[T]) {

  private val _1_to_log = 1 to log

  private def update(k: Int): Unit = {
    d(k) = m.combine(d(2 * k), d(2 * k + 1))
  }

  def set(index: Int, x: T): Unit = {
    assert(0 <= index && index < n)
    val p = index + size
    d(p) = x
    _1_to_log.foreach { i => { update(p >> i) } }
  }

  def get(index: Int): T = {
    assert(0 <= index && index < n)
    d(index + size)
  }

  def prod(range: Range): T = {
    val (l, r) = rightOpenInterval(range)
    prod(l, r)
  }

  def prod(left: Int, right: Int): T = {
    assert(0 <= left && left <= right && right <= n)
    var sml = m.e()
    var smr = m.e()
    var l = left + size
    var r = right + size

    while (l < r) {
      if ((l & 1) != 0) {
        sml = m.combine(sml, d(l))
        l += 1
      }
      if ((r & 1) != 0) {
        r -= 1
        smr = m.combine(d(r), smr)
      }
      l >>= 1
      r >>= 1
    }
    m.combine(sml, smr)
  }

  def allProd(): T = d(1)

  def maxRight(left: Int, f: T => Boolean): Int = {
    assert(0 <= left && left <= n)
    assert(f(m.e()))
    if (left == n) return n
    var l = left + size
    var sm = m.e()
    while ({
      // do
      while (l % 2 == 0) { l >>= 1 }
      if (!f(m.combine(sm, d(l)))) {
        while (l < size) {
          l *= 2
          if (f(m.combine(sm, d(l)))) {
            sm = m.combine(sm, d(l))
            l += 1
          }
        }
        return l - size
      }
      sm = m.combine(sm, d(l))
      l += 1

      // while
      (l & -l) != l
    }) {}
    n
  }

  def minLeft(right: Int, f: T => Boolean): Int = {
    assert(0 <= right && right <= n)
    assert(f(m.e()))
    if (right == 0) { return 0 }
    var r = right + size
    var sm = m.e()
    while ({
      // do
      r -= 1
      while (r > 1 && (r % 2) == 1) { r >>= 1 }
      if (!f(m.combine(d(r), sm))) {
        while (r < size) {
          r = 2 * r + 1
          if (f(m.combine(d(r), sm))) {
            sm = m.combine(d(r), sm)
            r -= 1
          }
        }
        return r + 1 - size
      }
      sm = m.combine(d(r), sm)

      // while
      (r & -r) != r
    }) {}
    0
  }

}

object Segtree {

  def apply[T](n: Int)(using m: Monoid[T], tag: ClassTag[T]): Segtree[T] = {
    val log = ceilPow2(n)
    val size = 1 << log
    val d = Array.fill(2 * size)(m.e())
    new Segtree(n, size, log, d)
  }

  def apply[T](array: Array[T])(using Monoid[T], ClassTag[T]): Segtree[T] = {
    val ret = Segtree(array.length)
    (0 until ret.n).foreach(i => { ret.d(ret.size + i) = array(i) })
    (1 until ret.size).reverse.foreach(ret.update)
    ret
  }

}
