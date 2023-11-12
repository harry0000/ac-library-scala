package io.github.acl4s

import scala.reflect.ClassTag

import io.github.acl4s.internal.{ceilPow2, rightOpenInterval, IPair}

final case class LazySegtree[S, F](
  n: Int
)(using m: Monoid[S], mm: MapMonoid[S, F], tagS: ClassTag[S], tagF: ClassTag[F]) {
  val log: Int = ceilPow2(n)
  val size: Int = 1 << log
  val d: Array[S] = Array.fill(2 * size)(m.e())
  val lz: Array[F] = Array.fill(size)(mm.id())

  private val _1_to_log = 1 to log
  private val _1_to_log_rev = _1_to_log.reverse

  private def update(k: Int): Unit = {
    d(k) = m.combine(d(2 * k), d(2 * k + 1))
  }

  private def applyAll(k: Int, f: F): Unit = {
    d(k) = mm.mapping(f, d(k))
    if (k < size) {
      lz(k) = mm.composition(f, lz(k))
    }
  }

  private def push(k: Int): Unit = {
    applyAll(2 * k, lz(k))
    applyAll(2 * k + 1, lz(k))
    lz(k) = mm.id()
  }

  def set(index: Int, x: S): Unit = {
    assert(0 <= index && index < n)
    val p = index + size
    _1_to_log_rev.foreach(i => { push(p >> i) })
    d(p) = x
    _1_to_log.foreach(i => { update(p >> i) })
  }

  def get(index: Int): S = {
    assert(0 <= index && index < n)
    val p = index + size
    _1_to_log_rev.foreach(i => { push(p >> i) })
    d(p)
  }

  def prod(range: Range): S = {
    val IPair(l, r) = rightOpenInterval(range)
    prod(l, r)
  }

  def prod(left: Int, right: Int): S = {
    assert(0 <= left && left <= right && right <= n)
    if (left == right) { return m.e() }
    var l = left + size
    var r = right + size
    _1_to_log_rev.foreach(i => {
      if (((l >> i) << i) != l) { push(l >> i) }
      if (((r >> i) << i) != r) { push((r - 1) >> i) }
    })
    var sml = m.e()
    var smr = m.e()
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

  def allProd: S = d(1)

  def applySingle(index: Int, f: F): Unit = {
    assert(0 <= index && index < n)
    val p = index + size
    _1_to_log_rev.foreach(i => { push(p >> i) })
    d(p) = mm.mapping(f, d(p))
    _1_to_log.foreach(i => { update(p >> i) })
  }

  def applyRange(range: Range, f: F): Unit = {
    val IPair(l, r) = rightOpenInterval(range)
    applyRange(l, r, f)
  }

  def applyRange(left: Int, right: Int, f: F): Unit = {
    assert(0 <= left && left <= right && right <= n)
    if (left == right) { return }
    var l = left + size
    var r = right + size
    _1_to_log_rev.foreach(i => {
      if (((l >> i) << i) != l) { push(l >> i) }
      if (((r >> i) << i) != r) { push((r - 1) >> i) }
    })

    {
      val l2 = l
      val r2 = r
      while (l < r) {
        if ((l & 1) != 0) {
          applyAll(l, f)
          l += 1
        }
        if ((r & 1) != 0) {
          r -= 1
          applyAll(r, f)
        }
        l >>= 1
        r >>= 1
      }
      l = l2
      r = r2
    }

    _1_to_log.foreach(i => {
      if (((l >> i) << i) != l) { update(l >> i) }
      if (((r >> i) << i) != r) { update((r - 1) >> i) }
    })
  }

  def maxRight(left: Int, g: S => Boolean): Int = {
    assert(0 <= left && left <= n)
    assert(g(m.e()))
    if (left == n) { return n }
    var l = left + size
    _1_to_log_rev.foreach(i => { push(l >> i) })
    var sm = m.e()
    while ({
      // do
      while (l % 2 == 0) { l >>= 1 }
      if (!g(m.combine(sm, d(l)))) {
        while (l < size) {
          push(l)
          l = 2 * l
          if (g(m.combine(sm, d(l)))) {
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

  def minLeft(right: Int, g: S => Boolean): Int = {
    assert(0 <= right && right <= n)
    assert(g(m.e()))
    if (right == 0) { return 0 }
    var r = right + size
    _1_to_log_rev.foreach(i => { push((r - 1) >> i) })
    var sm = m.e()
    while ({
      // do
      r -= 1
      while (r > 1 && (r % 2) == 1) { r >>= 1 }
      if (!g(m.combine(d(r), sm))) {
        while (r < size) {
          push(r)
          r = 2 * r + 1
          if (g(m.combine(d(r), sm))) {
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

object LazySegtree {

  def apply[S, F](n: Int)(using Monoid[S], MapMonoid[S, F], ClassTag[S], ClassTag[F]): LazySegtree[S, F] = {
    new LazySegtree(n)
  }

  def apply[S, F](array: Array[S])(using Monoid[S], MapMonoid[S, F], ClassTag[S], ClassTag[F]): LazySegtree[S, F] = {
    val ret = LazySegtree(array.length)
    (0 until ret.n).foreach(i => { ret.d(ret.size + i) = array(i) })
    (1 until ret.size).reverse.foreach(ret.update)
    ret
  }

}
