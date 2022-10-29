package org.justinhj.duct.typeclasses.comonad

import org.justinhj.duct.datatypes.{NonEmptyLazyList,NonEmptyList,CoReader}
import scala.collection.mutable.ListBuffer
import org.justinhj.duct.typeclasses.functor.given_Functor_NonEmptyList
import java.nio.charset.CoderResult
import org.justinhj.duct.typeclasses.functor.Functor

object Comonad:
  def apply[F[_]](using m: Comonad[F]) = m

trait Comonad[F[_]] extends Functor[F]:
    def extract[A](fa:F[A]):A

    extension[A,B](fa :F[A])
        def coflatMap(f: F[A] => B):F[B]

// Instance implementations

given nonEmptyListComonad: Comonad[NonEmptyList] with
  
    def extract[A](nel: NonEmptyList[A]) = nel.head

    extension [A, B](nel: NonEmptyList[A]) 
        def coflatMap(f: NonEmptyList[A] => B): NonEmptyList[B] = nel.tails.map(f)
        def map(f: A => B): NonEmptyList[B] = nel.map(f)

given coReaderComonad[R]: Comonad[[X] =>> CoReader[R,X]] with
    def extract[A](cr: CoReader[R,A]) = cr.value

    extension [A, B](cr: CoReader[R,A]) 
        override def coflatMap(f: CoReader[R,A] => B): CoReader[R,B] = cr.duplicate.map(f)
        def map(f: A => B): CoReader[R,B] = cr.map(f)

given nonEmptyLazyListComonad: Comonad[NonEmptyLazyList] with
  
    def extract[A](nel: NonEmptyLazyList[A]) = nel.head

    extension [A, B](nel: NonEmptyLazyList[A]) 
        def coflatMap(f: NonEmptyLazyList[A] => B): NonEmptyLazyList[B] = nel.tails.map(f)
        def map(f: A => B): NonEmptyLazyList[B] = nel.map(f)