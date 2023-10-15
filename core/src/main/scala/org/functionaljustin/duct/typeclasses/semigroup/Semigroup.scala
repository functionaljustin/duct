package org.functionaljustin.duct.typeclasses.semigroup

import org.functionaljustin.duct.datatypes.NonEmptyList

// Functional Justin https://youtu.be/v2TxejGEzg4
// Ep 8: Compose Yourself with Scala 3's Opaque Types

trait Semigroup[A]:
  extension (left: A)
    def combine(right: A): A
    def |+|(right: A): A = left.combine(right)  

object Semigroup:
  def apply[A](using s: Semigroup[A]) = s

given intSemigroup: Semigroup[Int] with
  extension (left: Int)
    def combine(right:Int) = left + right

given stringSemigroup: Semigroup[String] with
  extension (left: String)
    def combine(right:String) = left + right

given listSemigroup[A]: Semigroup[List[A]] with
  extension (left: List[A])
    def combine(right:List[A]) = left ++ right

given nonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] with
  extension (left: NonEmptyList[A])
    def combine(right:NonEmptyList[A]) = left.append(right)

given tuple2Semigroup[A: Semigroup,B: Semigroup]: Semigroup[Tuple2[A,B]] with
  extension (left: Tuple2[A,B])
    def combine(right:Tuple2[A,B]) = Tuple2(left._1 |+| right._1, left._2 |+| right._2)