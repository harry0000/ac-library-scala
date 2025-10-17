package io.github.acl4s

import io.github.acl4s.gcd

import scala.annotation.nowarn

// for ModInt imports
@nowarn("msg=unused import")
class ModIntSuite extends munit.FunSuite {

  test("DynamicModInt border") {
    import DynamicModInt as ModInt

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
        assertEquals(ModInt(a).pow(3).value, (((la * la) % lmod * la) % lmod).toInt)
        for (b <- v) {
          val lb = b.toLong
          assertEquals((ModInt(a) + ModInt(b)).value, ((la + lb) % lmod).toInt)
          assertEquals((ModInt(a) - ModInt(b)).value, ((la - lb + lmod) % lmod).toInt)
          assertEquals((ModInt(a) * ModInt(b)).value, ((la * lb) % lmod).toInt)
        }
      }
    })
  }

  test("DynamicModInt 1") {
    import DynamicModInt as ModInt

    ModInt.setMod(1)

    for {
      i <- 0 until 100
      j <- 0 until 100
    } {
      assertEquals((ModInt(i) * ModInt(j)).value, 0)
    }
    assertEquals((ModInt(12345) + ModInt(67890)).value, 0)
    assertEquals((ModInt(12345) - ModInt(67890)).value, 0)
    assertEquals((ModInt(12345) * ModInt(67890)).value, 0)
    assertEquals(ModInt(12345).pow(67890).value, 0)
    assertEquals(ModInt(0).inv.value, 0)
  }

  test("DynamicModInt inv") {
    import DynamicModInt as ModInt

    ModInt.setMod(998_244_353)
    (1 until 100_000).foreach(i => {
      val x = ModInt(i).inv.value
      assert(x >= 0)
      assert(x <= 998_244_353 - 1)
      assertEquals((x.toLong * i) % 998_244_353, 1L)
    })

    ModInt.setMod(1_000_000_008)
    (1 until 100_000).foreach(i => {
      if (gcd(i, 1_000_000_008) == 1) {
        val x = ModInt(i).inv.value
        assertEquals((x.toLong * i) % 1_000_000_008, 1L)
      }
    })

    ModInt.setMod(Int.MaxValue)
    (1 until 100_000).foreach(i => {
      if (gcd(i, Int.MaxValue) == 1) {
        val x = ModInt(i).inv.value
        assertEquals((x.toLong * i) % Int.MaxValue, 1L)
      }
    })
  }

  test("StaticModInt border") {
    {
      import ModInt998244353 as ModInt

      val mod = 998_244_353
      val lmod = mod.toLong

      (0 to 20).foreach(_ => {
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
          assertEquals(ModInt(a).pow(3).value, (((la * la) % lmod * la) % lmod).toInt)
          for (b <- v) {
            val lb = b.toLong
            assertEquals((ModInt(a) + ModInt(b)).value, ((la + lb) % lmod).toInt)
            assertEquals((ModInt(a) - ModInt(b)).value, ((la - lb + lmod) % lmod).toInt)
            assertEquals((ModInt(a) * ModInt(b)).value, ((la * lb) % lmod).toInt)
          }
        }
      })
    }
    {
      import ModInt1000000007 as ModInt

      val mod = 1_000_000_007
      val lmod = mod.toLong

      (0 to 20).foreach(_ => {
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
          assertEquals(ModInt(a).pow(3).value, (((la * la) % lmod * la) % lmod).toInt)
          for (b <- v) {
            val lb = b.toLong
            assertEquals((ModInt(a) + ModInt(b)).value, ((la + lb) % lmod).toInt)
            assertEquals((ModInt(a) - ModInt(b)).value, ((la - lb + lmod) % lmod).toInt)
            assertEquals((ModInt(a) * ModInt(b)).value, ((la * lb) % lmod).toInt)
          }
        }
      })
    }
  }

  test("StaticModInt 1") {
    import StaticModInt as ModInt

    given Modulus[1] = Modulus[1]()

    for {
      i <- 0 until 100
      j <- 0 until 100
    } {
      assertEquals((ModInt(i) * ModInt(j)).value, 0)
    }
    assertEquals((ModInt(12345) + ModInt(67890)).value, 0)
    assertEquals((ModInt(12345) - ModInt(67890)).value, 0)
    assertEquals((ModInt(12345) * ModInt(67890)).value, 0)
    assertEquals(ModInt(12345).pow(67890).value, 0)
    assertEquals(ModInt(0).inv.value, 0)
  }

  test("StaticModInt inv") {
    {
      import StaticModInt as ModInt

      given Modulus[11] = Modulus[11]()

      (1 until 11).foreach(i => {
        val x = ModInt(i).inv.value
        assertEquals((x * i) % 11, 1)
      })
    }
    {
      import StaticModInt as ModInt

      given Modulus[12] = Modulus[12]()

      (1 until 12).foreach(i => {
        if (gcd(i, 12) == 1) {
          val x = ModInt(i).inv.value
          assertEquals((x * i) % 12, 1)
        }
      })
    }
    {
      import StaticModInt as ModInt

      given Modulus[1_000_000_008] = Modulus[1_000_000_008]()

      (1 until 100_000).foreach(i => {
        if (gcd(i, 1_000_000_008) == 1) {
          val x = ModInt(i).inv.value
          assertEquals((x.toLong * i) % 1_000_000_008, 1L)
        }
      })
    }
    {
      import ModInt998244353 as ModInt

      (1 until 100_000).foreach(i => {
        val x = ModInt(i).inv.value
        assertEquals((x.toLong * i) % 998_244_353, 1L)
      })
    }
    {
      import ModInt1000000007 as ModInt

      (1 until 100_000).foreach(i => {
        val x = ModInt(i).inv.value
        assertEquals((x.toLong * i) % 1_000_000_007, 1L)
      })
    }
  }

}
