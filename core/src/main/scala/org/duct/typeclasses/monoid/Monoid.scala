package org.justinhj.typeclasses.monoid

import org.justinhj.typeclasses.semigroup.Semigroup

trait Monoid[A] extends Semigroup[A]:
  def zero: A

object Monoid:
  def apply[A](using m: Monoid[A]) = m

given stringMonoid: Monoid[String] with
  def zero = ""
  def combine(al:String, ar:String) = al + ar

given listMonoid[A]: Monoid[List[A]] with
  def zero = List.empty[A]
  def combine(al:List[A], ar:List[A]) = al ++ ar
