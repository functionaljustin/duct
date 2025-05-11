package org.functionaljustin.duct.typeclasses.comonad

import org.functionaljustin.duct.datatypes.{NonEmptyLazyList,NonEmptyList,CoReader}
import scala.collection.mutable.ListBuffer
import org.functionaljustin.duct.typeclasses.functor.given_Functor_NonEmptyList
import java.nio.charset.CoderResult
import org.functionaljustin.duct.typeclasses.functor.Functor

object Comonad:
  def apply[F[_]](using m: Comonad[F]) = m

trait Comonad[F[_]] extends Functor[F]:
    extension[A,B](fa :F[A])
        def extract: A
        def coflatMap(f: F[A] => B):F[B]

// Instance implementations

def last[A](nel: NonEmptyList[A]): A =
     if(nel.tail.isEmpty) 
        nel.head
     else
        nel.tail.last

def allButLast[A](nel: NonEmptyList[A]): NonEmptyList[A] =
  if(nel.tail.isEmpty) 
      nel
  else
      NonEmptyList(nel.head, nel.tail.init*)

def allButLasts[A](nel: NonEmptyList[A]): NonEmptyList[NonEmptyList[A]] = {
  def loop(curr: NonEmptyList[A], acc: List[NonEmptyList[A]]): NonEmptyList[NonEmptyList[A]] = {
    if (curr.tail.isEmpty) NonEmptyList(curr, acc*)
    else loop(allButLast(curr), curr :: acc)
  }
  loop(nel, Nil).reverse
}

given nonEmptyListComonad: Comonad[NonEmptyList] with
    extension [A, B](nel: NonEmptyList[A]) 
        def extract = last(nel)

        def coflatMap(f: NonEmptyList[A] => B): NonEmptyList[B] = 
            allButLasts(nel).map(f).reverse

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
