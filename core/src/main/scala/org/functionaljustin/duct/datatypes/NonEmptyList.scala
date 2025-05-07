package org.functionaljustin.duct.datatypes

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

// Ep 16: NonEmptyLists more or less
// https://youtu.be/7A2xuRkCZBg

// Note that I will do a future video on variance, so this is implemented as invariant for now. 
// If you don't know what that means, stay tuned!

/** NonEmptyList is a data structure that guarantees to contain at least one element.
 *
 * This implementation provides various operations that maintain the non-empty invariant,
 * making it safer to use in contexts where an empty list would cause errors.
 *
 * @tparam A the type of elements in this NonEmptyList
 * @param head the first element (guaranteed to exist)
 * @param tail the remaining elements (may be empty)
 */
case class NonEmptyList[A](head:A, tail:A*):
  /** Appends another NonEmptyList to this one.
   *
   * @param right the NonEmptyList to append
   * @return a new NonEmptyList containing all elements from this list followed by all elements from the right list
   */
  def append(right: NonEmptyList[A]): NonEmptyList[A] =
    NonEmptyList(head, (tail :+ right.head) ++ right.tail*)

  /** Returns a NonEmptyList containing all but the first element.
   *
   * @note This method is unsafe and will throw an exception if the tail is empty.
   * @return a NonEmptyList containing all but the first element
   */
  def tailUnsafe: NonEmptyList[A] = NonEmptyList(tail.head, tail.tail*)

  /** Safely attempts to get a NonEmptyList containing all but the first element.
   *
   * @return Some(NonEmptyList) if the tail has at least one element, None otherwise
   */
  def tailOption: Option[NonEmptyList[A]] = {
    tail match {
      case _ :: tl =>
        Some(NonEmptyList(tl.head, tl.tail*))
      case _ => None  
    }
  }

  /** Converts this NonEmptyList to a standard List.
   *
   * @return a List containing all elements in this NonEmptyList
   */
  def toList: List[A] = (head +: tail).toList

  /** Applies a function to each element and returns a new NonEmptyList.
   *
   * @tparam B the result type of the mapping function
   * @param f the function to apply to each element
   * @return a new NonEmptyList containing the results of applying f to each element
   */
  def map[B](f: A => B): NonEmptyList[B] = NonEmptyList(f(head), tail.map(f)*)

  /** Performs an action on each element of this NonEmptyList.
   *
   * @param f the function to apply to each element
   */
  def forEach(f: A => Unit): Unit =  {
    f(head)
    if tail.nonEmpty then
      tail.foreach(f)
  }

  /** Returns the number of elements in this NonEmptyList.
   *
   * @return the number of elements (always at least 1)
   */
  def size: Int = 1 + tail.size

  /** Folds this structure from left to right using the specified function.
   *
   * @tparam B the result type of the fold
   * @param z the initial value
   * @param f the binary function to apply
   * @return the final result after applying f to each element and the accumulated value
   */
  @tailrec
  final def foldLeft[B](z: B)(f: (B,A) => B): B =
    val next = f(z,head)
    tailOption match
      case Some(rest) => rest.foldLeft(next)(f)
      case None => next

  /** Returns all suffixes of this NonEmptyList, longest first.
   *
   * @return a NonEmptyList containing this list and all its non-empty suffixes
   */
  def tails: NonEmptyList[NonEmptyList[A]] = {
    var l = this.toList
    val tld = l.tails.filterNot(_.isEmpty).toList
    val nels = tld.map(n => NonEmptyList(n.head, n.tail*))
    NonEmptyList(nels.head, nels.tail*)
  }

  /** Returns a new NonEmptyList with elements in reverse order.
   *
   * @return a new NonEmptyList with all elements in reverse order
   */
  def reverse: NonEmptyList[A] = 
    val list = this.toList.reverse
    NonEmptyList(list.head, list.tail*)

/** Companion object for NonEmptyList providing factory methods.
 */
object NonEmptyList:
  /** Creates a NonEmptyList from a sequence if it's not empty.
   *
   * @tparam A the type of elements in the sequence
   * @param items the sequence to convert
   * @return Some(NonEmptyList) if the sequence is not empty, None otherwise
   */
  def fromSeq[A](items: Seq[A]): Option[NonEmptyList[A]] =
    if items.size > 0 then
      Some(NonEmptyList(items.head, items.tail*))
    else
      None
