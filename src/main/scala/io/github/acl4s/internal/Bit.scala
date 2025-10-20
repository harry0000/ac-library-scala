package io.github.acl4s.internal

def ceilPow2(n: Int): Int =
  32 - java.lang.Integer.numberOfLeadingZeros(0.max(n - 1))
