package io.github.acl4s

import scala.collection.mutable

/**
 * Implement (union by size) + (path compression)
 * Reference:
 * Zvi Galil and Giuseppe F. Italiano,
 * Data structures and algorithms for disjoint set union problems
 *
 * @param n
 */
case class Dsu(n: Int) {

  /**
   * root node: -1 * component size
   * otherwise: parent
   */
  private val parentOrSize: Array[Int] = Array.fill(n)(-1)

  def merge(a: Int, b: Int): Int = {
    assert(0 <= a && a < n)
    assert(0 <= b && b < n)
    var x = leader(a)
    var y = leader(b)
    if (x == y) { return x }
    if (-parentOrSize(x) < -parentOrSize(y)) {
      // std::swap(x, y);
      val z = x
      x = y
      y = z
    }
    parentOrSize(x) += parentOrSize(y)
    parentOrSize(y) = x
    x
  }

  def same(a: Int, b: Int): Boolean = {
    assert(0 <= a && a < n)
    assert(0 <= b && b < n)
    leader(a) == leader(b)
  }

  def leader(a: Int): Int = {
    assert(0 <= a && a < n)
    if (parentOrSize(a) < 0) {
      a
    } else {
      parentOrSize(a) = leader(parentOrSize(a))
      parentOrSize(a)
    }
  }

  def size(a: Int): Int = {
    assert(0 <= a && a < n)
    -parentOrSize(leader(a))
  }

  def groups(): collection.Seq[collection.Seq[Int]] = {
    val leader_buf = new Array[Int](n)
    val group_size = new Array[Int](n)
    (0 until n).foreach(i => {
      leader_buf(i) = leader(i)
      group_size(leader_buf(i)) += 1
    })

    val result = new mutable.ArrayBuffer[mutable.Buffer[Int]](n)
    (0 until n).foreach(i => {
      result += new mutable.ArrayBuffer(group_size(i))
    })
    (0 until n).foreach(i => {
      result(leader_buf(i)) += i
    })

    result.filter(_.nonEmpty)
  }

}
