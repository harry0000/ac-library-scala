package io.github.acl4s

trait MapMonoid[S: Monoid, F] {
  def e(): S = Monoid[S].e()
  def combine(a: S, b: S): S = Monoid[S].combine(a, b)

  def id(): F
  def mapping(f: F, s: S): S
  def composition(a: F, b: F): F
}

object MapMonoid
