package org.justinhj.typeclasses.semigroup

trait Semigroup[A]:
  def combine(al: A, ar: A): A

object Semigroup:
  def apply[A](using s: Semigroup[A]) = s

given stringSemigroup: Semigroup[String] with
  def combine(al:String, ar:String) = al + ar

given listSemigroup[A]: Semigroup[List[A]] with
  def combine(al:List[A], ar:List[A]) = al ++ ar
