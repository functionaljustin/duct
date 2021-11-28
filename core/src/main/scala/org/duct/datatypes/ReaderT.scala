package org.justinhj.datatypes

import org.justinhj.typeclasses.monad._

case class ReaderT[F[_],R,A](run: R => F[A])

object ReaderT:
  def lift[F[_],R,A](fa: F[A]): ReaderT[F,R,A] = ReaderT(_ => fa)

given readerTMonad[F[_]: Monad,R]: Monad[[A1] =>> ReaderT[F,R,A1]] with
  def pure[A](a:A): ReaderT[F,R,A] = ReaderT(_ => Monad[F].pure(a))

  extension [A,B](far: ReaderT[F,R,A])
    def flatMap(f: A => ReaderT[F,R,B]): ReaderT[F,R,B] = {
      ReaderT((r: R) =>
        val fa: F[A] = far.run(r)
        val fb: F[B] = fa.flatMap(b => f(b).run(r))
        fb
      )
    }

