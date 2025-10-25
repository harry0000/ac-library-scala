package io.github.acl4s

import scala.annotation.targetName

private inline def applyIntImpl(value: Int, mod: Int): Int = {
  var x = value % mod
  if (x < 0) { x += mod }
  x
}

private inline def applyLongImpl(value: Long, mod: Int): Long = {
  var x = value % mod
  if (x < 0L) { x += mod }
  x
}

private inline def addImpl(lhs: Int, rhs: Int, mod: Int): Int = {
  var v = lhs.toLong + rhs
  if (v >= mod) {
    v -= mod
  }
  v.toInt
}

private inline def subImpl(lhs: Int, rhs: Int, mod: Int): Int = {
  var v = lhs - rhs
  if (v < 0) {
    v += mod
  }
  v
}

sealed trait Modulus[T <: Int] {
  def value: T
  def isPrime: Boolean
}
case object Mod1000000007 extends Modulus[1_000_000_007] {
  override val value: 1_000_000_007 = 1_000_000_007
  override val isPrime: Boolean = true
}
case object Mod998244353 extends Modulus[998_244_353] {
  override val value: 998_244_353 = 998_244_353
  override val isPrime: Boolean = true
}
object Modulus {
  final private case class Mod[T <: Int](value: T) extends Modulus[T] {
    override val isPrime: Boolean = internal.isPrime(value)
  }

  inline def apply[T <: Int](): Modulus[T] = Mod(compiletime.constValue[T])
}

opaque type StaticModInt[T <: Int] = Int

object StaticModInt {
  inline def apply[T <: Int]()(using Modulus[T]): StaticModInt[T] = 0

  inline def apply[T <: Int](value: Int)(using m: Modulus[T]): StaticModInt[T] = {
    val x = applyIntImpl(value, m.value)
    raw(x)
  }

  inline def apply[T <: Int](value: Long)(using m: Modulus[T]): StaticModInt[T] = {
    val x = applyLongImpl(value, m.value)
    raw(x.toInt)
  }

  private inline def raw[T <: Int](value: Int): StaticModInt[T] = value

  private inline def mul[T <: Int](a: StaticModInt[T], b: StaticModInt[T])(using m: Modulus[T]): StaticModInt[T] = {
    val mod = m.value.toLong
    val res = (a.toLong * b.toLong) % mod
    raw(res.toInt)
  }

  extension [T <: Int](self: StaticModInt[T])(using m: Modulus[T]) {
    // We use `value`. `val` is a reserved word in Scala.
    inline def value: Int = self

    @targetName("add")
    inline def +(rhs: StaticModInt[T]): StaticModInt[T] = {
      raw(addImpl(self, rhs, m.value))
    }

    @targetName("sub")
    inline def -(rhs: StaticModInt[T]): StaticModInt[T] = {
      raw(subImpl(self, rhs, m.value))
    }

    @targetName("mul")
    inline def *(rhs: StaticModInt[T]): StaticModInt[T] = {
      mul(self, rhs)
    }

    @targetName("div")
    inline def /(rhs: StaticModInt[T]): StaticModInt[T] = {
      mul(self, rhs.inv)
    }

    inline def pow(n: Int): StaticModInt[T] = {
      var _n = n
      var x = self
      var r = apply(1)
      while (_n > 0) {
        if ((_n & 1) == 1) {
          r = mul(r, x)
        }
        x = mul(x, x)
        _n >>= 1
      }
      r
    }

    inline def inv: StaticModInt[T] = {
      if (m.isPrime) {
        assert(self != 0)
        pow(m.value - 2)
      } else {
        val (g, x) = internal.invGcd(self.toLong, m.value.toLong)
        assert(g == 1L)
        apply(x)
      }
    }
  }
}

type ModInt1000000007 = StaticModInt[Mod1000000007.value.type]
type ModInt998244353 = StaticModInt[Mod998244353.value.type]

given mod1000000007Modulus: Modulus[Mod1000000007.value.type] = Mod1000000007
given mod998244353Modulus: Modulus[Mod998244353.value.type] = Mod998244353

object ModInt1000000007 {
  def apply(): ModInt1000000007 = StaticModInt()
  def apply(value: Int): ModInt1000000007 = StaticModInt(value)
  def apply(value: Long): ModInt1000000007 = StaticModInt(value)
}

object ModInt998244353 {
  def apply(): ModInt998244353 = StaticModInt()
  def apply(value: Int): ModInt998244353 = StaticModInt(value)
  def apply(value: Long): ModInt998244353 = StaticModInt(value)
}

given [T <: Int](using Modulus[T]): Conversion[Int, StaticModInt[T]] with {
  def apply(i: Int): StaticModInt[T] = StaticModInt(i)
}

given [T <: Int](using Modulus[T]): Conversion[Long, StaticModInt[T]] with {
  def apply(l: Long): StaticModInt[T] = StaticModInt(l)
}

opaque type DynamicModInt = Int

object DynamicModInt {
  private var bt = internal.Barrett(-1)
  private def mod: Int = bt.m

  def setMod(mod: Int): Unit = {
    require(1 <= mod)
    bt = internal.Barrett(mod)
  }

  inline def apply(): DynamicModInt = 0

  inline def apply(value: Int): DynamicModInt = {
    val x = applyIntImpl(value, bt.m)
    raw(x)
  }

  inline def apply(value: Long): DynamicModInt = {
    val x = applyLongImpl(value, bt.m)
    raw(x.toInt)
  }

  private inline def raw(value: Int): DynamicModInt = value

  private inline def mul(a: DynamicModInt, b: DynamicModInt): DynamicModInt = {
    raw(bt.mul(a, b))
  }

  extension (self: DynamicModInt) {
    // We use `value`. `val` is a reserved word in Scala.
    inline def value: Int = self

    @targetName("add")
    inline def +(rhs: DynamicModInt): DynamicModInt = {
      raw(addImpl(self, rhs, mod))
    }

    @targetName("sub")
    inline def -(rhs: DynamicModInt): DynamicModInt = {
      raw(subImpl(self, rhs, mod))
    }

    @targetName("mul")
    inline def *(rhs: DynamicModInt): DynamicModInt = {
      mul(self, rhs)
    }

    @targetName("div")
    inline def /(rhs: DynamicModInt): DynamicModInt = {
      mul(self, rhs.inv)
    }

    inline def pow(n: Int): DynamicModInt = {
      var _n = n
      var x = self
      var r = raw(1)
      while (_n > 0) {
        if ((_n & 1) == 1) {
          r = mul(r, x)
        }
        x = mul(x, x)
        _n >>= 1
      }
      r
    }

    inline def inv: DynamicModInt = {
      val (g, x) = internal.invGcd(self.toLong, mod.toLong)
      assert(g == 1L)
      apply(x)
    }
  }
}

given Conversion[Int, DynamicModInt] with {
  def apply(i: Int): DynamicModInt = DynamicModInt(i)
}

given Conversion[Long, DynamicModInt] with {
  def apply(l: Long): DynamicModInt = DynamicModInt(l)
}
