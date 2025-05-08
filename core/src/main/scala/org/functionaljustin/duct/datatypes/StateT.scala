package org.functionaljustin.duct.datatypes

import org.functionaljustin.duct.typeclasses.monad.Monad
import org.functionaljustin.duct.typeclasses.functor.Functor
import org.functionaljustin.duct.typeclasses.applicative.Applicative
import org.functionaljustin.duct.typeclasses.monoid.{given,_}

/** A monad transformer that adds state handling capabilities to another monad.
 *
 * The StateT transformer allows composition of stateful operations with effects from the underlying monad.
 * It represents computations that can read from and write to a state value while also performing
 * effects described by the base monad F.
 */
object StateT:
  /** Lifts a value in the underlying monad into the StateT monad transformer.
   *
   * This function takes a monadic value and a state, and transforms them into a StateT
   * where the original state is preserved and the monadic value becomes the result.
   *
   * @tparam F the underlying monad
   * @tparam S the state type
   * @tparam A the result type
   * @param fa the monadic value to lift
   * @param s the initial state
   * @param F evidence that F has an Applicative instance
   * @return a StateT that wraps the input monadic value with the provided state
   */
  def lift[F[_],S, A](fa: F[A], s: S)(using F: Applicative[F]): StateT[F,S,A] =
    StateT(F.map(fa)(a => (s,a)))

/** A monad transformer that combines state operations with effects from another monad.
 *
 * StateT is a wrapper around an F[(S,A)] where:
 * - F is the underlying monad (like Option, Either, etc.)
 * - S is the state type
 * - A is the result type
 *
 * It represents a computation that takes a state of type S and returns:
 * - A new state of type S
 * - A result of type A
 * - Wrapped in the effect context F
 *
 * @tparam F the underlying monad
 * @tparam S the state type
 * @tparam A the result type
 * @param wrapped the wrapped monadic value containing a tuple of state and result
 */
case class StateT[F[_],S,A](val wrapped: F[(S,A)])
