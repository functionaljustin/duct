package org.justinhj.duct.typeclasses.semigroup

import org.justinhj.duct.datatypes.NonEmptyList

// Functional Justin https://youtu.be/v2TxejGEzg4
// Ep 8: Compose Yourself with Scala 3's Opaque Types

trait Semigroup[A]:
  extension (left: A)
    def combine(right: A): A
    def |+|(right: A): A = left.combine(right)  

object Semigroup:
  def apply[A](using s: Semigroup[A]) = s

given stringSemigroup: Semigroup[String] with
  extension (left: String)
    def combine(right:String) = left + right

given listSemigroup[A]: Semigroup[List[A]] with
  extension (left: List[A])
    def combine(right:List[A]) = left ++ right

given nonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] with
  extension (left: NonEmptyList[A])
      def combine(right: NonEmptyList[A]) = left.append(right)