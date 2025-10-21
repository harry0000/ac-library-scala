package io.github.acl4s

import scala.reflect.ClassTag

import io.github.acl4s.internal.{foreach, IPair}

/**
 * Reference: https://en.wikipedia.org/wiki/Fenwick_tree
 *
 * @param n
 * @param m
 * @tparam T
 */
final class FenwickTree[T: ClassTag](
  private val n: Int
)(using m: AddSub[T]) {
  private val data: Array[T] = Array.fill(n)(m.e())

  def this(array: Array[T])(using AddSub[T]) = {
    this(array.length)
    foreach(array.indices)(i => { add(i, array(i)) })
  }

  def add(index: Int, x: T): Unit = {
    require(0 <= index && index < n)
    var p = index + 1
    while (p <= n) {
      data(p - 1) = m.combine(data(p - 1), x)
      p += p & -p
    }
  }

  private def sum(i: Int): T = {
    var s = m.e()
    var r = i
    while (r > 0) {
      s = m.combine(s, data(r - 1))
      r -= r & -r
    }
    s
  }

  def sum(range: Range): T = {
    val IPair(l, r) = internal.rightOpenInterval(range)
    sum(l, r)
  }

  def sum(l: Int, r: Int): T = {
    require(0 <= l && l <= r && r <= n)
    m.subtract(sum(r), sum(l))
  }
}

trait AddSub[T] extends Add[T] {
  def subtract(a: T, b: T): T
}

object AddSub {
  given (using m: Add[Char]): AddSub[Char] with {
    override def e(): Char = m.e()
    override def combine(a: Char, b: Char): Char = m.combine(a, b)
    override def subtract(a: Char, b: Char): Char = (a - b).asInstanceOf[Char]
  }

  given (using m: Add[Byte]): AddSub[Byte] with {
    override def e(): Byte = m.e()
    override def combine(a: Byte, b: Byte): Byte = m.combine(a, b)
    override def subtract(a: Byte, b: Byte): Byte = (a - b).asInstanceOf[Byte]
  }

  given (using m: Add[Short]): AddSub[Short] with {
    override def e(): Short = m.e()
    override def combine(a: Short, b: Short): Short = m.combine(a, b)
    override def subtract(a: Short, b: Short): Short = (a - b).asInstanceOf[Short]
  }

  given (using m: Add[Int]): AddSub[Int] with {
    override def e(): Int = m.e()
    override def combine(a: Int, b: Int): Int = m.combine(a, b)
    override def subtract(a: Int, b: Int): Int = a - b
  }

  given (using m: Add[Long]): AddSub[Long] with {
    override def e(): Long = m.e()
    override def combine(a: Long, b: Long): Long = m.combine(a, b)
    override def subtract(a: Long, b: Long): Long = a - b
  }

  given (using m: Add[Float]): AddSub[Float] with {
    override def e(): Float = m.e()
    override def combine(a: Float, b: Float): Float = m.combine(a, b)
    override def subtract(a: Float, b: Float): Float = a - b
  }

  given (using m: Add[Double]): AddSub[Double] with {
    override def e(): Double = m.e()
    override def combine(a: Double, b: Double): Double = m.combine(a, b)
    override def subtract(a: Double, b: Double): Double = a - b
  }

  given (using m: Add[DynamicModInt]): AddSub[DynamicModInt] with {
    override def e(): DynamicModInt = m.e()
    override def combine(a: DynamicModInt, b: DynamicModInt): DynamicModInt = m.combine(a, b)
    override def subtract(a: DynamicModInt, b: DynamicModInt): DynamicModInt = a - b
  }

  given (using m: Add[ModInt998244353]): AddSub[ModInt998244353] with {
    override def e(): ModInt998244353 = m.e()
    override def combine(a: ModInt998244353, b: ModInt998244353): ModInt998244353 = m.combine(a, b)
    override def subtract(a: ModInt998244353, b: ModInt998244353): ModInt998244353 = a - b
  }

  given (using m: Add[ModInt1000000007]): AddSub[ModInt1000000007] with {
    override def e(): ModInt1000000007 = m.e()
    override def combine(a: ModInt1000000007, b: ModInt1000000007): ModInt1000000007 = m.combine(a, b)
    override def subtract(a: ModInt1000000007, b: ModInt1000000007): ModInt1000000007 = a - b
  }

  given [T <: Int](using m: Add[StaticModInt[T]], _mod: Modulus[T]): AddSub[StaticModInt[T]] with {
    override def e(): StaticModInt[T] = m.e()
    override def combine(a: StaticModInt[T], b: StaticModInt[T]): StaticModInt[T] = m.combine(a, b)
    override def subtract(a: StaticModInt[T], b: StaticModInt[T]): StaticModInt[T] = a - b
  }
}
