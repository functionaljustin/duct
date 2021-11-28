package org.justinhj.typeclasses.applicative

import org.justinhj.typeclasses.functor.Functor
import org.justinhj.typeclasses.monoid._
import org.justinhj.datatypes.WriterT

object Applicative:
  def apply[F[_]](using a: Applicative[F]) = a

trait Applicative[F[_]] extends Functor[F]:
  
  def pure[A](x:A):F[A]

  extension [A,B](x: F[A]) 
    def ap(f: F[A => B]): F[B]

    def map(f: A => B): F[B] = {
      x.ap(pure(f))
    }
    
  extension [A,B,C](fa: F[A]) def map2(fb: F[B])(f: (A,B) => C): F[C] = {
    val fab: F[B => C] = fa.map((a: A) => (b: B) => f(a,b))
    fb.ap(fab)
  }

end Applicative

given eitherApplicative[Err]: Applicative[[X] =>> Either[Err,X]] with {
  def pure[A](a: A): Either[Err, A] = Right(a)
  
  extension [A,B](x: Either[Err,A]) def ap(f: Either[Err, A => B]) = {
    (f,x) match {
      case (Right(f), Right(x)) => Right(f(x)) 
      case (Left(err), _) => Left(err)
      case (_, Left(err)) => Left(err)
    }
  }
}

given optionApplicative: Applicative[Option] with {
  def pure[A](a: A): Option[A] = Option(a)
  
  extension[A,B](fa: Option[A])
    def ap(ff: Option[A => B]): Option[B] = {
      (fa,ff) match {
        case (Some(a), Some(f)) => Some(f(a))
        case _ => None  
      }
    }
}

given listApplicative: Applicative[List] with {
  def pure[A](a: A): List[A] = List(a)

  extension[A,B](as: List[A])
    def ap(fs: List[A => B]): List[B] = {
      fs match {
        case f :: tl => as.map(f) ++ as.ap(tl)
        case Nil => Nil
      }
    }
}

given writerTApplicative[F[_]: Applicative,W: Monoid]: Applicative[[X] =>> WriterT[F,W,X]] with {

  def pure[A](a: A): WriterT[F,W,A] = WriterT(Applicative[F].pure((Monoid[W].zero,a)))

  extension [A,B](fa: WriterT[F,W,A]) 
    def ap(ff: WriterT[F,W,A => B]): WriterT[F,W,B] = {
      WriterT(Applicative[F].ap(fa.unwrap())(ff.unwrap().map(
        (aw,af) => 
          (in: (W,A)) => ((Monoid[W].combine(in._1,aw)),af(in._2)))))
    }
  }