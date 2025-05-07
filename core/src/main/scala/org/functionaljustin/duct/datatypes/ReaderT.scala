package org.functionaljustin.duct.datatypes

import org.functionaljustin.duct.typeclasses.monad._

/** ReaderT is a monad transformer that adds the ability to read from a shared environment to a base monad F.
 *
 * ReaderT represents a computation that reads values from a shared environment of type R,
 * and returns values wrapped in a monad F.
 *
 * @tparam F the base monad
 * @tparam R the type of the environment/context
 * @tparam A the type of the result
 * @param run the function that, given an environment of type R, returns a value of type F[A]
 */
case class ReaderT[F[_],R,A](run: R => F[A])

/** Companion object for ReaderT providing factory methods.
 */
object ReaderT:
  /** Lifts a value in the base monad F into ReaderT.
   *
   * @tparam F the base monad
   * @tparam R the type of the environment/context
   * @tparam A the type of the value
   * @param fa the value in the base monad to lift
   * @return a ReaderT that ignores its environment and returns the lifted value
   */
  def lift[F[_],R,A](fa: F[A]): ReaderT[F,R,A] = ReaderT(_ => fa)

/** Provides a Monad instance for ReaderT.
 *
 * This allows ReaderT to be used with for-comprehensions and other constructs
 * that require a Monad instance.
 *
 * @tparam F the base monad
 * @tparam R the type of the environment/context
 */
given readerTMonad[F[_]: Monad,R]: Monad[[A1] =>> ReaderT[F,R,A1]] with
  /** Creates a ReaderT that ignores its environment and returns the given value.
   *
   * @tparam A the type of the value
   * @param a the value to return
   * @return a ReaderT that returns the given value wrapped in the base monad
   */
  def pure[A](a:A): ReaderT[F,R,A] = ReaderT(_ => Monad[F].pure(a))

  extension [A,B](far: ReaderT[F,R,A])
    /** Chains two ReaderT computations, with the result of the first feeding into the second.
     *
     * @param f a function that takes the result of the first computation and returns a new ReaderT
     * @return a new ReaderT representing the sequential composition of the two computations
     */
    def flatMap(f: A => ReaderT[F,R,B]): ReaderT[F,R,B] = {
      ReaderT((r: R) =>
        val fa: F[A] = far.run(r)
        val fb: F[B] = fa.flatMap(b => f(b).run(r))
        fb
      )
    }

