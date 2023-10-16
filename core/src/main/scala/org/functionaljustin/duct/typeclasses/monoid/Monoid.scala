package org.functionaljustin.duct.typeclasses.monoid

import org.functionaljustin.duct.typeclasses.semigroup.Semigroup

// Functional Justin https://youtu.be/v2TxejGEzg4
// Ep 8: Compose Yourself with Scala 3's Opaque Types

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
