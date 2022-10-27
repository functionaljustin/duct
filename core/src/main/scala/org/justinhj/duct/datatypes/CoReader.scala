package org.justinhj.duct.datatypes

case class CoReader[R,A](value: A, ask: R) {
  def map[B](f: A => B): CoReader[R,B] = {
    CoReader(f(value), ask)
  }
  def extract: A = value
  def coflatMap[B](f: CoReader[R,A] => B): CoReader[R,B] = {
    duplicate.map(f)
  }
  def duplicate: CoReader[R, CoReader[R,A]] = {
    CoReader(this, ask)
  }
}
