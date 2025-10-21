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

private inline def addImpl[T <: Int](lhs: T, rhs: T, mod: Int): Int = {
  var v = lhs.toLong + rhs
  if (v >= mod) {
    v -= mod
  }
  v.toInt
}

private inline def subImpl[T <: Int](lhs: T, rhs: T, mod: Int): Int = {
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
case class Mod[T <: Int](value: T) extends Modulus[T] {
  override val isPrime: Boolean = internal.isPrime(value)
}
object Modulus {
  inline def apply[T <: Int](): Modulus[T] = Mod(compiletime.constValue[T])
}

final case class StaticModInt[T <: Int] private (private var _value: Int)(using m: Modulus[T]) {
  type Self = StaticModInt[T]
  val mod: T = m.value

  def value: Int = _value
  protected def raw(value: Int): StaticModInt[T] = new StaticModInt(value)
  protected def apply(value: Int): StaticModInt[T] = StaticModInt.apply(value)

  private inline def mulImpl(lhs: Int, rhs: Int): Long = {
    (lhs.toLong * rhs.toLong) % mod.toLong
  }

  @targetName("addAssign")
  def +=(rhs: Self): Unit = {
    _value = addImpl(this.value, rhs.value, mod)
  }

  @targetName("subAssign")
  def -=(rhs: Self): Unit = {
    _value = subImpl(this.value, rhs.value, mod)
  }

  @targetName("mulAssign")
  def *=(rhs: Self): Unit = {
    _value = mulImpl(value, rhs.value).toInt
  }

  @targetName("divAssign")
  def /=(rhs: Self): Unit = {
    _value = mulImpl(value, rhs.inv.value).toInt
  }

  @targetName("add")
  def +(rhs: Self): Self = {
    raw(addImpl(this.value, rhs.value, mod))
  }

  @targetName("sub")
  def -(rhs: Self): Self = {
    raw(subImpl(this.value, rhs.value, mod))
  }

  @targetName("mul")
  def *(rhs: Self): Self = {
    raw(mulImpl(value, rhs.value).toInt)
  }

  @targetName("div")
  def /(rhs: Self): Self = {
    this * rhs.inv
  }

  def pow(n: Int): Self = {
    var _n = n
    var x = this
    val r = raw(1)
    while (_n > 0) {
      if ((_n & 1) == 1) {
        r *= x
      }
      x = x * x
      _n >>= 1
    }
    r
  }

  def inv: Self = {
    if (m.isPrime) {
      assert(_value != 0)
      pow(mod - 2)
    } else {
      val (g, x) = internal.invGcd(_value.toLong, mod.toLong)
      assert(g == 1L)
      apply(x.toInt)
    }
  }
}

object StaticModInt {
  def apply[T <: Int]()(using m: Modulus[T]): StaticModInt[T] = {
    new StaticModInt(0)
  }

  def apply[T <: Int](value: Int)(using m: Modulus[T]): StaticModInt[T] = {
    val x = applyIntImpl(value, m.value)
    new StaticModInt(x)
  }

  def apply[T <: Int](value: Long)(using m: Modulus[T]): StaticModInt[T] = {
    val x = applyLongImpl(value, m.value)
    new StaticModInt(x.toInt)
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

final case class DynamicModInt private (private var _value: Int) {
  type Self = DynamicModInt
  val mod: Int = DynamicModInt.bt.m

  def value: Int = _value
  protected def raw(value: Int): DynamicModInt = new DynamicModInt(value)
  protected def apply(value: Int): DynamicModInt = DynamicModInt.apply(value)

  @targetName("addAssign")
  def +=(rhs: Self): Unit = {
    _value = addImpl(this.value, rhs.value, mod)
  }

  @targetName("subAssign")
  def -=(rhs: Self): Unit = {
    _value = subImpl(this.value, rhs.value, mod)
  }

  @targetName("mulAssign")
  def *=(rhs: Self): Unit = {
    _value = DynamicModInt.bt.mul(value, rhs.value)
  }

  @targetName("divAssign")
  def /=(rhs: Self): Unit = {
    _value = DynamicModInt.bt.mul(value, rhs.inv.value)
  }

  @targetName("add")
  def +(rhs: Self): Self = {
    raw(addImpl(this.value, rhs.value, mod))
  }

  @targetName("sub")
  def -(rhs: Self): Self = {
    raw(subImpl(this.value, rhs.value, mod))
  }

  @targetName("mul")
  def *(rhs: Self): Self = {
    raw(DynamicModInt.bt.mul(value, rhs.value))
  }

  @targetName("div")
  def /(rhs: Self): Self = {
    this * rhs.inv
  }

  def pow(n: Int): Self = {
    var _n = n
    var x = this
    val r = raw(1)
    while (_n > 0) {
      if ((_n & 1) == 1) {
        r *= x
      }
      x = x * x
      _n >>= 1
    }
    r
  }

  def inv: Self = {
    val (g, x) = internal.invGcd(_value.toLong, mod.toLong)
    assert(g == 1L)
    apply(x.toInt)
  }
}

object DynamicModInt {
  private var bt = internal.Barrett(-1)

  def setMod(mod: Int): Unit = {
    require(1 <= mod)
    bt = internal.Barrett(mod)
  }

  def apply(): DynamicModInt = {
    new DynamicModInt(0)
  }

  def apply(value: Int): DynamicModInt = {
    val x = applyIntImpl(value, bt.m)
    new DynamicModInt(x)
  }

  def apply(value: Long): DynamicModInt = {
    val x = applyLongImpl(value, bt.m)
    new DynamicModInt(x.toInt)
  }
}
