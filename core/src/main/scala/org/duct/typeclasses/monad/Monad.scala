package org.justinhj.typeclasses.monad

import org.justinhj.typeclasses.applicative.Applicative
import org.justinhj.typeclasses.functor.Functor
import org.justinhj.datatypes._
import org.justinhj.typeclasses.monoid.{given, _}

object Monad:
  def apply[F[_]](using m: Monad[F]) = m

trait Monad[F[_]] extends Applicative[F]:

 // The unit value for a monad
 def pure[A](x:A):F[A]

 extension[A,B](fa :F[A])
   // The fundamental composition operation
   def flatMap(f :A=>F[B]):F[B]

   // Monad can also implement `ap` in terms of `map` and `flatMap`
   def ap(fab: F[A => B]): F[B] = {
     fab.flatMap {
       f =>
         fa.flatMap {
           a =>
             pure(f(a))
         }
     }

   }

end Monad

given eitherMonad[Err]: Monad[[X] =>> Either[Err,X]] with
  def pure[A](a: A): Either[Err, A] = Right(a)
  extension [A,B](x: Either[Err,A]) def flatMap(f: A => Either[Err, B]) = {
    x match {
      case Right(a) => f(a)
      case Left(err) => Left(err)
    }
  }

given optionMonad: Monad[Option] with
  def pure[A](a: A) = Some(a)
  extension[A,B](fa: Option[A])
    def flatMap(f: A => Option[B]) = {
      fa match {
        case Some(a) =>
          f(a)
        case None =>
          None
      }
    }

given listMonad: Monad[List] with
  def pure[A](a: A): List[A] = List(a)

  extension[A,B](x: List[A])
    def flatMap(f: A => List[B]): List[B] = {
      x match {
        case hd :: tl => f(hd) ++ tl.flatMap(f)
        case Nil => Nil
      }
    }

given writerTMonad[F[_]: Monad,W: Monoid]: Monad[[X] =>> WriterT[F,W,X]] with {

  def pure[A](a: A): WriterT[F,W,A] = WriterT(Monad[F].pure((Monoid[W].zero,a)))

  extension [A,B](fa: WriterT[F,W,A]) def flatMap(f: A => WriterT[F,W,B]) = {
     val ffa: F[(W,B)] = Monad[F].flatMap(fa.unwrap()) {
       case (wa,a) => {
         f(a).unwrap().map {
           case (wb, b) =>
             (Monoid[W].combine(wa,wb), b)
         }
       }
     }
     WriterT(ffa)
   }
  }

// This is needed to help the typer with monad transformers
// see https://github.com/lampepfl/dotty/issues/11413#
extension [F[_], A](fa: F[A])(using m: Monad[F])
  def flatMap[B](f: A => F[B]) = m.flatMap(fa)(f)