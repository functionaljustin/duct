package org.duct.typeclasses.semigroup

trait Semigroup[A]:
  extension (left: A)
    def combine(right: A): A

object Semigroup:
  def apply[A](using s: Semigroup[A]) = s

given stringSemigroup: Semigroup[String] with
  extension (left: String)
    def combine(right:String) = left + right

given listSemigroup[A]: Semigroup[List[A]] with
  extension (left: List[A])
    def combine(right:List[A]) = left ++ right
