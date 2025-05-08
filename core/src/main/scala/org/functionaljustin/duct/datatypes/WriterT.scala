package org.functionaljustin.duct.datatypes

import org.functionaljustin.duct.typeclasses.monad.Monad
import org.functionaljustin.duct.typeclasses.functor.Functor
import org.functionaljustin.duct.typeclasses.applicative.Applicative
import org.functionaljustin.duct.typeclasses.monoid.{given,_}

/** Companion object for WriterT that provides factory methods.
  * Functional Justin https:*youtu.be/KMm71HLoy0w
  * Ep 9: Transformers, Monads in Disguise
  * Working with Monad Transformers can be kinda awful. In particular 
  * I ran into many type inference issues creating this code. That's
  * what that PartiallyApplied stuff is for in the pure and lift functions
  * below...
 */
object WriterT:
  /** Creates a partially applied function for lifting values into WriterT.
   * @tparam W the type of the log/output value
   * @return a partially applied function that can lift values into WriterT
   */
  def lift[W]: LiftPartiallyApplied[W] = new LiftPartiallyApplied[W]

  /** Helper class for handling type inference when lifting values into WriterT.
   *
   * @tparam W the type of the log/output value
   */
  class LiftPartiallyApplied[W](private val dummy: Boolean = true) extends AnyVal {
    /** Lifts a value in the base monad F into WriterT with an empty log.
     *
     * @tparam F the base monad
     * @tparam A the type of the value
     * @param fa the value in the base monad to lift
     * @param f evidence that F is a Monad
     * @param m evidence that W forms a Monoid
     * @return a WriterT with an empty log and the lifted value
     */
    def apply[F[_],A](fa: F[A])(using f: Monad[F], m: Monoid[W]): WriterT[F, W, A] = 
      WriterT(f.map(fa)(a => (m.zero, a)))
  }

  /** Creates a partially applied function for creating pure WriterT values.
   *
   * @tparam F the base monad
   * @tparam W the type of the log/output value
   * @return a partially applied function that can create pure WriterT values
   */
  def pure[F[_],W]: PurePartiallyApplied[F,W] = new PurePartiallyApplied[F,W]

  /** Helper class for handling type inference when creating pure WriterT values.
   *
   * @tparam F the base monad
   * @tparam W the type of the log/output value
   */
  class PurePartiallyApplied[F[_],W](private val dummy: Boolean = true) extends AnyVal {
    /** Creates a WriterT that contains a pure value and an empty log.
     *
     * @tparam B the type of the value
     * @param b the value to wrap
     * @param F evidence that F is a Monad
     * @param m evidence that W forms a Monoid
     * @return a WriterT with an empty log and the given value
     */
    def apply[B](b: B)(using F: Monad[F], m: Monoid[W]): WriterT[F, W, B] = WriterT(F.pure((m.zero,b)))
  } 

/** WriterT is a monad transformer that adds the ability to accumulate an output value (log) to a base monad F.
 *
 * WriterT represents a computation that produces a value of type A along with an accumulated output value of type W,
 * all wrapped in a monad F.
 *
 * @tparam F the base monad
 * @tparam W the type of the log/output value that can be accumulated
 * @tparam A the type of the result
 * @param wrapped the underlying value in the base monad, containing both the log and result
 */
case class WriterT[F[_],W,A](val wrapped: F[(W,A)]):
  /** Appends a value to the log without changing the result value.
   *
   * @param l1 the value to append to the log
   * @param m evidence that W forms a Monoid
   * @param f evidence that F is a Functor
   * @return a WriterT with the updated log and the same result value
   */
  def tell(l1: W)(using m: Monoid[W], f: Functor[F]): WriterT[F,W,A] =
    WriterT(wrapped.map{
      (l2,a) =>
        (l2.combine(l1), a)
    })

  /** Appends a value to the log based on the current result value.
   *
   * @param faw a function that transforms the result into a value to append to the log
   * @param m evidence that W forms a Monoid
   * @param f evidence that F is a Functor
   * @return a WriterT with the updated log and the same result value
   */
  def tellWith(faw: A => W)(using m: Monoid[W], f: Functor[F]): WriterT[F,W,A] =
    WriterT(wrapped.map{
      (l2,a) =>
        (l2.combine(faw(a)), a)
    })
    
  /** Extracts just the log from this WriterT.
   *
   * @param f evidence that F is a Functor
   * @return the log value wrapped in the base monad
   */
  def written(using f: Functor[F]): F[W] =
    f.map(wrapped)(_._1)

  /** Extracts just the result value from this WriterT, discarding the log.
   *
   * @param f evidence that F is a Functor
   * @return the result value wrapped in the base monad
   */
  def value(using f: Functor[F]): F[A] =
    f.map(wrapped)(_._2)
