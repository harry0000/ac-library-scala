package io.github.acl4s

import io.github.acl4s.gcd

class ModIntSuite extends BaseSuite {

  test("DynamicModInt border") {
    val ModInt = DynamicModInt

    (0 to 20).foreach(d => {
      val mod = Int.MaxValue - d
      val lmod = mod.toLong
      ModInt.setMod(mod)
      val v = (0 until 10).flatMap(i =>
        Array(
          i,
          mod - i,
          mod / 2 + i,
          mod / 2 - i
        )
      )
      for (a <- v) {
        val la = a.toLong
        assert(ModInt(a).pow(3).value === (((la * la) % lmod * la) % lmod).toInt)
        for (b <- v) {
          val lb = b.toLong
          assert((ModInt(a) + ModInt(b)).value === ((la + lb) % lmod).toInt)
          assert((ModInt(a) - ModInt(b)).value === ((la - lb + lmod) % lmod).toInt)
          assert((ModInt(a) * ModInt(b)).value === ((la * lb) % lmod).toInt)
        }
      }
    })
  }

  test("DynamicModInt 1") {
    val ModInt = DynamicModInt
    ModInt.setMod(1)

    for {
      i <- 0 until 100
      j <- 0 until 100
    } {
      assert((ModInt(i) * ModInt(j)).value === 0)
    }
    assert((ModInt(12345) + ModInt(67890)).value === 0)
    assert((ModInt(12345) - ModInt(67890)).value === 0)
    assert((ModInt(12345) * ModInt(67890)).value === 0)
    assert(ModInt(12345).pow(67890).value === 0)
    assert(ModInt(0).inv.value === 0)
  }

  test("DynamicModInt inv") {
    val ModInt = DynamicModInt
    ModInt.setMod(998_244_353)
    (1 until 100_000).foreach(i => {
      val x = ModInt(i).inv.value
      assert(x >= 0)
      assert(x <= 998_244_353 - 1)
      assert((x.toLong * i) % 998_244_353 === 1L)
    })

    ModInt.setMod(1_000_000_008)
    (1 until 100_000).foreach(i => {
      if (gcd(i, 1_000_000_008) == 1) {
        val x = ModInt(i).inv.value
        assert((x.toLong * i) % 1_000_000_008 === 1L)
      }
    })

    ModInt.setMod(Int.MaxValue)
    (1 until 100_000).foreach(i => {
      if (gcd(i, Int.MaxValue) == 1) {
        val x = ModInt(i).inv.value
        assert((x.toLong * i) % Int.MaxValue === 1L)
      }
    })
  }

  test("StaticModInt border") {
    {
      val ModInt = ModInt998244353
      val mod = 998_244_353
      val lmod = mod.toLong

      (0 to 20).foreach(d => {
        val v = (0 until 10).flatMap(i =>
          Array(
            i,
            mod - i,
            mod / 2 + i,
            mod / 2 - i
          )
        )
        for (a <- v) {
          val la = a.toLong
          assert(ModInt(a).pow(3).value === (((la * la) % lmod * la) % lmod).toInt)
          for (b <- v) {
            val lb = b.toLong
            assert((ModInt(a) + ModInt(b)).value === ((la + lb) % lmod).toInt)
            assert((ModInt(a) - ModInt(b)).value === ((la - lb + lmod) % lmod).toInt)
            assert((ModInt(a) * ModInt(b)).value === ((la * lb) % lmod).toInt)
          }
        }
      })
    }
    {
      val ModInt = ModInt1000000007
      val mod = 1_000_000_007
      val lmod = mod.toLong

      (0 to 20).foreach(d => {
        val v = (0 until 10).flatMap(i =>
          Array(
            i,
            mod - i,
            mod / 2 + i,
            mod / 2 - i
          )
        )
        for (a <- v) {
          val la = a.toLong
          assert(ModInt(a).pow(3).value === (((la * la) % lmod * la) % lmod).toInt)
          for (b <- v) {
            val lb = b.toLong
            assert((ModInt(a) + ModInt(b)).value === ((la + lb) % lmod).toInt)
            assert((ModInt(a) - ModInt(b)).value === ((la - lb + lmod) % lmod).toInt)
            assert((ModInt(a) * ModInt(b)).value === ((la * lb) % lmod).toInt)
          }
        }
      })
    }
  }

  test("StaticModInt 1") {
    given Modulus[1] = Modulus[1]()
    val ModInt = StaticModInt

    for {
      i <- 0 until 100
      j <- 0 until 100
    } {
      assert((ModInt(i) * ModInt(j)).value === 0)
    }
    assert((ModInt(12345) + ModInt(67890)).value === 0)
    assert((ModInt(12345) - ModInt(67890)).value === 0)
    assert((ModInt(12345) * ModInt(67890)).value === 0)
    assert(ModInt(12345).pow(67890).value === 0)
    assert(ModInt(0).inv.value === 0)
  }

  test("StaticModInt inv") {
    {
      given Modulus[11] = Modulus[11]()
      val ModInt = StaticModInt

      (1 until 11).foreach(i => {
        val x = ModInt(i).inv.value
        assert((x * i) % 11 === 1)
      })
    }
    {
      given Modulus[12] = Modulus[12]()
      val ModInt = StaticModInt

      (1 until 12).foreach(i => {
        if (gcd(i, 12) == 1) {
          val x = ModInt(i).inv.value
          assert((x * i) % 12 === 1)
        }
      })
    }
    {
      given Modulus[1_000_000_008] = Modulus[1_000_000_008]()
      val ModInt = StaticModInt

      (1 until 100_000).foreach(i => {
        if (gcd(i, 1_000_000_008) == 1) {
          val x = ModInt(i).inv.value
          assert((x.toLong * i) % 1_000_000_008 === 1L)
        }
      })
    }
    {
      val ModInt = ModInt998244353

      (1 until 100_000).foreach(i => {
        val x = ModInt(i).inv.value
        assert((x.toLong * i) % 998_244_353 === 1L)
      })
    }
    {
      val ModInt = ModInt1000000007

      (1 until 100_000).foreach(i => {
        val x = ModInt(i).inv.value
        assert((x.toLong * i) % 1_000_000_007 === 1L)
      })
    }
  }

}
