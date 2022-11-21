package org.justinhj.duct.typeclasses.comonad

import org.justinhj.duct.datatypes.{NonEmptyLazyList,NonEmptyList,CoReader}
import scala.collection.mutable.ListBuffer
import org.justinhj.duct.typeclasses.functor.given_Functor_NonEmptyList
import java.nio.charset.CoderResult
import org.justinhj.duct.typeclasses.functor.Functor

object Comonad:
  def apply[F[_]](using m: Comonad[F]) = m

trait Comonad[F[_]] extends Functor[F]:
    extension[A,B](fa :F[A])
        def extract: A
        def coflatMap(f: F[A] => B):F[B]

// Instance implementations

given nonEmptyListComonad: Comonad[NonEmptyList] with
  
    extension [A, B](nel: NonEmptyList[A]) 
        def extract = nel.head

        def coflatMap(f: NonEmptyList[A] => B): NonEmptyList[B] = nel.tails.map(f)
        def map(f: A => B): NonEmptyList[B] = nel.map(f)

given coReaderComonad[R]: Comonad[[X] =>> CoReader[R,X]] with
    extension [A, B](cr: CoReader[R,A]) 
        def extract = cr.extract

        override def coflatMap(f: CoReader[R,A] => B): CoReader[R,B] = 
           cr.duplicate.map(f)
        def map(f: A => B): CoReader[R,B] = 
            cr.coflatMap(cra => f(cra.extract))

given nonEmptyLazyListComonad: Comonad[NonEmptyLazyList] with
    extension [A, B](nel: NonEmptyLazyList[A]) 
        def extract = nel.head
        def coflatMap(f: NonEmptyLazyList[A] => B): NonEmptyLazyList[B] = nel.tails.map(f)
        def map(f: A => B): NonEmptyLazyList[B] = nel.map(f)