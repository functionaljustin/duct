package org.functionaljustin.duct.datatypes

/** CoReader is a comonadic data structure that combines a value with an environment.
 *
 * @tparam R the type of the environment/context
 * @tparam A the type of the contained value
 * @param extract the value stored in this CoReader
 * @param ask the environment/context associated with this CoReader
 */
case class CoReader[R,A](extract: A, ask: R):
  /** Maps the contained value using the provided function.
   *
   * @tparam B the resulting type after applying the function
   * @param f the function to apply to the contained value
   * @return a new CoReader with the transformed value and the same environment
   */
  def map[B](f: A => B): CoReader[R,B] = CoReader(f(extract),ask)
  
  /** Creates a nested CoReader structure.
   *
   * @return a CoReader containing the original CoReader as its value
   */
  def duplicate: CoReader[R, CoReader[R,A]] = CoReader(this, ask)
