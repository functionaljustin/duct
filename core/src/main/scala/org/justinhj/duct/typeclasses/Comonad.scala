package org.justinhj.duct.typeclasses

import org.justinhj.duct.datatypes.NonEmptyList
import scala.collection.mutable.ListBuffer
import org.justinhj.duct.typeclasses.functor.given_Functor_NonEmptyList

object Comonad:
  def apply[F[_]](using m: Comonad[F]) = m

trait Comonad[F[_]]:
    def extract[A](fa:F[A]):A

    extension[A,B](fa :F[A])
        def coFlatMap(f: F[A] => B):F[B]

// Instance implementations

given nonEmptyListComonad: Comonad[NonEmptyList] with
    def extract[A](nel: NonEmptyList[A]) = nel.head

    extension [A, B](nel: NonEmptyList[A]) 
        override def coFlatMap(f: NonEmptyList[A] => B): NonEmptyList[B] = nel.tails.map(f)