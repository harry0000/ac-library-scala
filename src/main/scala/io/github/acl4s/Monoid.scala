package io.github.acl4s

trait Monoid[T] {
  def e(): T
  def combine(a: T, b: T): T
}

object Monoid {
  def apply[T](using m: Monoid[T]): Monoid[T] = m
}

trait Add[T] extends Monoid[T]
object Add {
  given Add[Char] with {
    override def e(): Char = 0
    override def combine(a: Char, b: Char): Char = (a + b).asInstanceOf[Char]
  }

  given Add[Byte] with {
    override def e(): Byte = 0
    override def combine(a: Byte, b: Byte): Byte = (a + b).asInstanceOf[Byte]
  }

  given Add[Short] with {
    override def e(): Short = 0
    override def combine(a: Short, b: Short): Short = (a + b).asInstanceOf[Short]
  }

  given Add[Int] with {
    override def e(): Int = 0
    override def combine(a: Int, b: Int): Int = a + b
  }

  given Add[Long] with {
    override def e(): Long = 0L
    override def combine(a: Long, b: Long): Long = a + b
  }

  given Add[Float] with {
    override def e(): Float = 0f
    override def combine(a: Float, b: Float): Float = a + b
  }

  given Add[Double] with {
    override def e(): Double = 0d
    override def combine(a: Double, b: Double): Double = a + b
  }

  given Add[DynamicModInt] with {
    override def e(): DynamicModInt = DynamicModInt()
    override def combine(a: DynamicModInt, b: DynamicModInt): DynamicModInt = a + b
  }

  given Add[ModInt998244353] with {
    override def e(): ModInt998244353 = ModInt998244353()
    override def combine(a: ModInt998244353, b: ModInt998244353): ModInt998244353 = a + b
  }

  given Add[ModInt1000000007] with {
    override def e(): ModInt1000000007 = ModInt1000000007()
    override def combine(a: ModInt1000000007, b: ModInt1000000007): ModInt1000000007 = a + b
  }

  given [T <: Int](using Modulus[T]): Add[StaticModInt[T]] with {
    override def e(): StaticModInt[T] = StaticModInt()
    override def combine(a: StaticModInt[T], b: StaticModInt[T]): StaticModInt[T] = a + b
  }

  def apply[T](using m: Add[T]): Add[T] = m
}
