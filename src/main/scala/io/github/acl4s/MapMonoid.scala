package io.github.acl4s

trait MapMonoid[S, F] {
  def id(): F
  def mapping(f: F, s: S): S
  def composition(a: F, b: F): F
}

object MapMonoid
