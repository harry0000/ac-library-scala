package io.github.acl4s

trait Monoid[T] {
  def e(): T
  def combine(a: T, b: T): T
}

object Monoid {
  def apply[T](using m: Monoid[T]): Monoid[T] = m
}
