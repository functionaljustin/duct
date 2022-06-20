package org.duct.typeclasses.monoid

import org.duct.typeclasses.semigroup.Semigroup

trait Monoid[A] extends Semigroup[A]:
  def zero: A

object Monoid:
  def apply[A](using m: Monoid[A]) = m

given stringMonoid: Monoid[String] with
  def zero = ""
  extension (al: String)
      def combine(ar:String) = al + ar

given listMonoid[A]: Monoid[List[A]] with
  def zero = List.empty[A]
  extension (al: List[A])
    def combine(ar:List[A]) = al ++ ar
