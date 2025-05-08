package org.functionaljustin.duct.datatypes

import scala.annotation.tailrec

/** A non-empty lazily evaluated list data structure.
 * 
 * NonEmptyLazyList provides a collection that is guaranteed to have at least one element,
 * with the rest of the elements evaluated only when needed. This data structure is useful
 * when you need to ensure that a list contains at least one element while still maintaining
 * lazy evaluation behavior.
 *
 * A nonempty implementation of lazy list. No video for this one but it is used in the Comonad
 * video since you can't make a full Comonad instance for lazy list. The implementation is a
 * straightforward nonempty handling interface on top of LazyList
 * 
 * @tparam A the type of elements in the non-empty lazy list
 */
sealed trait NonEmptyLazyList[+A]:
    /** Returns the first element of this non-empty lazy list.
     * 
     * @return the first element
     */
    def head: A
    
    /** Returns a lazy list consisting of all elements except the first.
     * 
     * @return the tail of this list as a LazyList (which may be empty)
     */
    def tail: LazyList[A]

    /** Returns the tail of this non-empty lazy list as an Option.
     * 
     * @return Some(tail) as a NonEmptyLazyList if the tail has at least one element,
     *         None if the tail is empty
     */
    def tailOption: Option[NonEmptyLazyList[A]] =
      if tail.isEmpty then None
      else Some(NonEmptyLazyList.cons(tail.head, tail.tail))

    /** Converts this non-empty lazy list to a lazy list.
     * 
     * @return a LazyList containing all elements of this non-empty lazy list
     */
    def toLazyList: LazyList[A] =
      LazyList.cons(head, tail)

    /** Builds a new non-empty lazy list by applying a function to all elements of this list.
     * 
     * @tparam B the element type of the returned list
     * @param f the function to apply to each element
     * @return a new non-empty lazy list resulting from applying f to each element
     */
    def map[B](f: A => B): NonEmptyLazyList[B] =
      NonEmptyLazyList.cons(f(head), tail.map(f))

    /** Applies a function f to all elements of this non-empty lazy list.
     * 
     * @param f the function that is applied for its side-effect to every element
     */
    @tailrec
    final def forEach(f: A => Unit): Unit =
      f(head)
      this.tailOption match
        case Some(tail) => tail.forEach(f)
        case None => ()

    /** Returns all suffixes of this non-empty lazy list.
     * 
     * @return a non-empty lazy list of all suffixes of this list, starting with this
     *         list itself
     */
    def tails: NonEmptyLazyList[NonEmptyLazyList[A]] =
      NonEmptyLazyList.cons(this, 
        this.tailOption match
          case Some(tail) => 
            tail.tails.toLazyList
          case None => LazyList.empty)

    /** Returns a non-empty lazy list formed from this list and another list
     * by combining corresponding elements in pairs.
     * 
     * @tparam B the type of the second list elements
     * @param other the second non-empty lazy list
     * @return a new non-empty lazy list containing pairs of corresponding elements
     */
    def zip[B](other: NonEmptyLazyList[B]): NonEmptyLazyList[(A,B)] =
      NonEmptyLazyList.cons((this.head,other.head), this.tail.zip(other.tail))

    /** Sums up the elements of this non-empty lazy list.
     * 
     * @tparam B a supertype of A that has Numeric type class instance
     * @param num numeric type class instance
     * @return the sum of all elements
     */
    def sum[B >: A](implicit num: Numeric[B]): B = this.toLazyList.sum

    /** Converts this non-empty lazy list to a standard List.
     * 
     * @return a new List containing all elements of this non-empty lazy list
     */
    def toList: List[A] = this.toLazyList.toList

    /** Selects first n elements of this non-empty lazy list.
     * 
     * @param n the number of elements to take
     * @return a non-empty lazy list consisting of the first n elements,
     *         or the whole list if n > size
     * @throws NoSuchElementException if n <= 0
     */
    def take(n: Int): NonEmptyLazyList[A] =
      val ll = this.toLazyList.take(n)
      NonEmptyLazyList.cons(ll.head, ll.tail)        

/** Factory methods and operations for non-empty lazy lists.
 */
object NonEmptyLazyList:
    /** Constructs a non-empty lazy list from a head element and a tail.
     * 
     * @tparam A the type of the list elements
     * @param hd the head element (guaranteed to exist)
     * @param tl the tail of the list (may be empty)
     * @return a new non-empty lazy list with the given head and tail
     */
    def cons[A](hd: => A, tl: => LazyList[A]) = new NonEmptyLazyList[A]:
        lazy val head = hd
        lazy val tail = tl

    /** Creates a non-empty lazy list containing the given elements.
     * 
     * @tparam A the type of the list elements
     * @param a the first element of the list (guaranteed to exist)
     * @param as additional elements of the list (may be empty)
     * @return a non-empty lazy list containing the given elements
     */
    def apply[A](a: => A, as: A*): NonEmptyLazyList[A] =
        NonEmptyLazyList.cons(a, LazyList.apply(as*))

    /** Creates an infinite non-empty lazy list where all elements are the same value.
     * 
     * @tparam A the type of the list elements
     * @param a the element to repeat
     * @return an infinite non-empty lazy list containing only the element a
     */
    def repeat[A](a: A): NonEmptyLazyList[A] = NonEmptyLazyList.cons(a, LazyList.repeat(a))
    
    /** Creates an infinite non-empty lazy list where the first element is given,
     * and the next elements are generated by repeatedly applying a function.
     * 
     * @tparam A the type of the list elements
     * @param a the first element
     * @param next the function to generate the next element
     * @return an infinite non-empty lazy list where each element is derived from the previous one
     */
    def iterate[A](a: A)(next: A => A): NonEmptyLazyList[A] = NonEmptyLazyList.cons(a,LazyList.iterate(next(a))(next))
