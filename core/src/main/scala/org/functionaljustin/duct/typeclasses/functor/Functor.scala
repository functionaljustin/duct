package org.functionaljustin.duct.typeclasses.functor

import org.functionaljustin.duct.datatypes._

object Functor:
  def apply[F[_]](using f: Functor[F]) = f

trait Functor[F[_]]:
  extension [A, B](x: F[A])
    def map(f: A => B): F[B]

given Functor[List] with
  extension[A,B](x: List[A])
    def map(f: A => B): List[B] = {
      x match {
        case hd :: tl => f(hd) :: tl.map(f)
        case Nil => Nil
      }
    }

given eitherFunctor[Err]: Functor[[X] =>> Either[Err,X]] with
  extension[A,B](x: Either[Err,A]) def map(f: A => B) = x match {
    case Right(a) => Right(f(a))
    case Left(err) => Left(err)
  }

given Functor[NonEmptyList] with
  extension[A,B](x: NonEmptyList[A])
    def map(f: A => B): NonEmptyList[B] = {
      val mapped = x.toList.map(f)
      NonEmptyList(mapped.head, mapped.tail*)
    }
