package org.justinhj.duct.datatypes

// Functional Justin https://youtu.be/KMm71HLoy0w
// Ep 9: Transformers, Monads in Disguise
// Working with Monad Transformers can be kinda awful. In particular 
// I ran into many type inference issues creating this code. That's
// what that PartiallyApplied stuff is for in the pure and lift functions
// below...

import org.justinhj.duct.typeclasses.monad.Monad
import org.justinhj.duct.typeclasses.functor.Functor
import org.justinhj.duct.typeclasses.applicative.Applicative
import org.justinhj.duct.typeclasses.monoid.{given,_}

object WriterT:
  def lift[W]: LiftPartiallyApplied[W] = new LiftPartiallyApplied[W]

  class LiftPartiallyApplied[W](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_],A](fa: F[A])(using f: Monad[F], m: Monoid[W]): WriterT[F, W, A] = 
      WriterT(f.map(fa)(a => (m.zero, a)))
  }

  def pure[F[_],W]: PurePartiallyApplied[F,W] = new PurePartiallyApplied[F,W]

  class PurePartiallyApplied[F[_],W](private val dummy: Boolean = true) extends AnyVal {
    def apply[B](b: B)(using F: Monad[F], m: Monoid[W]): WriterT[F, W, B] = WriterT(F.pure((m.zero,b)))
  } 

case class WriterT[F[_],W,A](val wrapped: F[(W,A)]):
  // tell let's us write to the log without affecting the current computed value
  def tell(l1: W)(using m: Monoid[W], f: Functor[F]): WriterT[F,W,A] =
    WriterT(wrapped.map{
      (l2,a) =>
        (l2.combine(l1), a)
    })

  // tellWith let's us write to the log without affecting the current computed value
  // and we can access the value when making the log
  def tellWith(faw: A => W)(using m: Monoid[W], f: Functor[F]): WriterT[F,W,A] =
    WriterT(wrapped.map{
      (l2,a) =>
        (l2.combine(faw(a)), a)
    })
    
  // written is so you can grab the log
  def written(using f: Functor[F]): F[W] =
    f.map(wrapped)(_._1)

  // value is so you can grab the value and drop the log
  def value(using f: Functor[F]): F[A] =
    f.map(wrapped)(_._2)
