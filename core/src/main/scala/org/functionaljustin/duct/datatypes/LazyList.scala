package org.functionaljustin.duct.datatypes

import scala.annotation.tailrec
import math.Numeric.Implicits.infixNumericOps

/** A lazily evaluated list data structure.
 * 
 * LazyList provides a collection that evaluates its elements only when needed,
 * allowing for potentially infinite sequences. Operations on lazy lists use
 * non-strict evaluation, deferring computation until absolutely necessary.
 *
 * Code developed in the video https://youtu.be/laB15gG5bjY Ep 17: The Magic of LazyLists
 * This is a simple as possible implementation of LazyList based roughly on the original 
 * Scala stdlib version from 2003 by Odersky.
 *
 * @tparam A the type of elements in the lazy list
 */
sealed trait LazyList[+A] {
    /** Returns the first element of this lazy list.
     * 
     * @throws NoSuchElementException if the list is empty
     * @return the first element of this lazy list
     */
    def head: A
    
    /** Returns a lazy list consisting of all elements except the first.
     * 
     * @throws UnsupportedOperationException if the list is empty
     * @return the tail of this lazy list
     */
    def tail: LazyList[A]
    
    /** Tests whether this lazy list is empty.
     * 
     * @return true if the list contains no elements, false otherwise
     */
    def isEmpty: Boolean

    /** Returns the first element of this lazy list as an Option.
     * 
     * @return Some(head) if the list is non-empty, None if it's empty
     */
    def headOption = if !isEmpty then     
        Some(head) 
      else 
        None

    /** Returns the tail of this lazy list as an Option.
     * 
     * @return Some(tail) if the list has a non-empty tail, None otherwise
     */
    def tailOption = if isEmpty then None
        else 
            if tail.isEmpty then None
            else Some(tail)

    /** Builds a new lazy list by applying a function to all elements of this lazy list.
     * 
     * @tparam B the element type of the returned lazy list
     * @param f the function to apply to each element
     * @return a new lazy list resulting from applying f to each element
     */
    def map[B](f: A => B): LazyList[B] =
        if isEmpty then LazyList.empty
        else LazyList.cons(f(head), tail.map(f))

    /** Returns all suffixes of this lazy list.
     * 
     * @return a lazy list of all suffixes of this lazy list, starting with this
     *         lazy list itself
     */
    def tails: LazyList[LazyList[A]] =
      if isEmpty then LazyList.empty
      else LazyList.cons(this, tail.tails)

    /** Builds a new lazy list by applying a function to all suffixes of this lazy list.
     * 
     * This is the cobind operation for the comonad instance of LazyList.
     * 
     * @tparam B the element type of the returned lazy list
     * @param f the function to apply to each suffix
     * @return a new lazy list resulting from applying f to each suffix
     */
    def coflatMap[B](f: LazyList[A] => B): LazyList[B] = {
      this.tails.map(f)
    }

    /** Returns a lazy list formed from this lazy list and another lazy list
     * by combining corresponding elements in pairs.
     * 
     * @tparam B the type of the second lazy list elements
     * @param other the second lazy list
     * @return a new lazy list containing pairs consisting of corresponding elements of this and that
     */
    def zip[B](other: LazyList[B]): LazyList[(A,B)] = {
        if isEmpty || other.isEmpty then LazyList.empty
        else LazyList.cons((head, other.head), tail.zip(other.tail))
    }

    /** Removes all elements from the prefix of this lazy list that satisfy the given predicate.
     * 
     * @param f the predicate used to test elements
     * @return the longest suffix of this lazy list whose first element does not satisfy the predicate
     */
    @tailrec
    final def dropWhile(f: A => Boolean): LazyList[A] = {
        if isEmpty then LazyList.empty
        else if f(head) then tail.dropWhile(f)
        else this
    }

    /** Selects all elements of this lazy list which satisfy a predicate.
     * 
     * @param f the predicate used to test elements
     * @return a new lazy list consisting of all elements of this lazy list that satisfy the given predicate
     */
    def filter(f: A => Boolean): LazyList[A] = {
        val dropped = this.dropWhile(a => !f(a))
        if dropped.isEmpty then LazyList.empty
        else LazyList.cons(dropped.head, dropped.tail.filter(f))    
    }    

    /** Selects all elements except first n ones.
     * 
     * @param n the number of elements to drop
     * @return a lazy list consisting of all elements of this lazy list except the first n ones, or empty if n > length
     */
    @tailrec
    final def drop(n: Int): LazyList[A] = {
        if n == 0 || isEmpty then this
        else tail.drop(n-1)
    }

    /** Selects first n elements of this lazy list.
     * 
     * @param n the number of elements to take
     * @return a lazy list consisting of the first n elements of this lazy list, or the whole list if n > length
     */
    final def take(n: Int): LazyList[A] = {
        if n == 0 || isEmpty then LazyList.empty
        else LazyList.cons(head, tail.take(n-1))
    } 

    /** Tests whether a predicate holds for at least one element of this lazy list.
     * 
     * @param f the predicate used to test elements
     * @return true if at least one element satisfies the predicate, false otherwise
     */
    def exists(f: A => Boolean): Boolean = {
        this.first(f).isDefined
    }

    /** Tests whether a predicate holds for all elements of this lazy list.
     * 
     * @param f the predicate used to test elements
     * @return true if all elements satisfy the predicate, false otherwise
     */
    @tailrec    
    final def forall(f: A => Boolean): Boolean = {
        if isEmpty then true
        else f(head) && tail.forall(f)
    }

    /** Converts this lazy list to a list.
     * 
     * @return a new list containing all elements of this lazy list
     */
    def toList: List[A] = {
        if isEmpty then List.empty[A]
        else head +: tail.toList
    }

    /** Applies a function f to all elements of this lazy list.
     * 
     * @param f the function that is applied for its side-effect to every element
     */
    @tailrec
    final def forEach(f: A => Unit): Unit = {
        if !isEmpty then
            f(head)
            tail.forEach(f)
    }

    /** Applies a binary operator to a start value and all elements of this lazy list,
     * going left to right.
     * 
     * @tparam B the result type of the binary operator
     * @param z the start value
     * @param f the binary operator
     * @return the result of inserting f between consecutive elements of this lazy list,
     *         going left to right, with the start value z on the left
     */
    @tailrec
    final def foldLeft[B](z: B)(f: (B, A) => B): B = {
        if isEmpty then z
        else tail.foldLeft(f(z, head))(f)
    }

    /** Applies a binary operator to a start value and all elements of this lazy list,
     * going right to left.
     * 
     * @tparam B the result type of the binary operator
     * @param z the start value
     * @param f the binary operator
     * @return the result of inserting f between consecutive elements of this lazy list,
     *         going right to left, with the start value z on the right
     */
    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
        if isEmpty then z
        else
            f(head, tail.foldRight(z)(f))
    }

    /** Partitions this lazy list into a pair of lazy lists according to a predicate.
     * 
     * @param f the predicate on which to partition
     * @return a pair of lazy lists: the first containing all elements that satisfy the predicate,
     *         the second containing all elements that don't
     */
    def partition(f: A => Boolean): (LazyList[A],LazyList[A]) = {
      (filter(f), filter(a => !f(a)))
    }

    /** Finds the first element of the lazy list satisfying a predicate, if any.
     * 
     * @param f the predicate used to test elements
     * @return an option containing the first element satisfying the predicate, or None if none exists
     */
    def first(f: A => Boolean) = this.filter(f).headOption

    /** Sums up the elements of this lazy list.
     * 
     * @tparam B a supertype of A that has Numeric type class instance
     * @param num numeric type class instance
     * @return the sum of all elements
     */
    def sum[B >: A](implicit num: Numeric[B]): B =
        this.foldLeft(Numeric[B].zero) {
            (acc,n) =>
                acc + n
        }

    /** Concatenates this lazy list with another.
     * 
     * @tparam BB the element type of the returned lazy list
     * @param e the lazy list to append
     * @return a lazy list containing elements from this followed by elements from e
     */
    def ++[BB >: A](e: => LazyList[BB]): LazyList[BB] =
        foldRight(e){
            case (hd,acc) =>
                LazyList.cons(hd, acc)
        }
}

/** Factory methods and operations for lazy lists.
 */
object LazyList:
    /** The empty lazy list.
     */
    val empty = new LazyList[Nothing] {
        def head = throw new NoSuchElementException("Cannot get head of empty lazy list")
        def tail = throw new UnsupportedOperationException("No tail of empty lazy list")
        val isEmpty = true
    }

    /** Constructs a lazy list from a head element and a tail.
     * 
     * @tparam A the type of the lazy list elements
     * @param hd the head element
     * @param tl the tail of the list
     * @return a new lazy list with the given head and tail
     */
    def cons[A](hd: => A, tl: => LazyList[A]) = new LazyList[A] {
        lazy val head = hd
        lazy val tail = tl
        val isEmpty = false
    }    

    /** Creates a lazy list containing the given elements.
     * 
     * @tparam A the type of the lazy list elements
     * @param as the elements of the created lazy list
     * @return a lazy list containing the given elements
     */
    def apply[A](as: A*): LazyList[A] = {
        if as.isEmpty then LazyList.empty
        else LazyList.cons(as.head, apply(as.tail*))
    }    

    /** Provides the cons operator for LazyList construction.
     * 
     * @tparam A the type of the element and the lazy list
     * @param hd the head element
     */
    extension [A](hd: => A)
        /** Prepends an element to a LazyList.
         * 
         * @param tl the lazy list to prepend to
         * @return a new lazy list with hd as the first element and tl as the rest
         */
        def #::(tl: => LazyList[A]): LazyList[A] = 
            LazyList.cons(hd, tl)

    /** Creates an infinite lazy list where all elements are the same value.
     * 
     * @tparam A the type of the lazy list elements
     * @param a the element to repeat
     * @return an infinite lazy list containing only the element a
     */
    def repeat[A](a: A): LazyList[A] = a #:: repeat(a)
    
    /** Creates an infinite lazy list where the first element is given,
     * and the next elements are generated by repeatedly applying a function.
     * 
     * @tparam A the type of the lazy list elements
     * @param a the first element
     * @param next the function to generate the next element
     * @return an infinite lazy list where each element is derived from the previous one
     */
    def iterate[A](a: A)(next: A => A): LazyList[A] = a #:: iterate(next(a))(next)

    /** Creates an infinite lazy list of consecutive integers starting at n.
     * 
     * @param n the first element
     * @return an infinite lazy list of integers starting at n
     */
    def from(n: Int) : LazyList[Int] = n #:: from(n+1)

    /** Creates a lazy list of consecutive integers from begin (inclusive) to end (exclusive).
     * 
     * @param begin the first element of the list
     * @param end one past the last element of the list
     * @return a lazy list of integers from begin to end-1
     */
    def range(begin: Int, end: Int): LazyList[Int] = {
        def helper(n: Int, end: Int): LazyList[Int] = {
            if n == end then LazyList.empty
            else LazyList.cons(n, helper(n + 1, end))
        }
        helper(begin,end)
    }

    /** Builds a lazy list from a seed value and a function for generating
     * both the next element and the next seed value.
     * 
     * @tparam A the type of the lazy list elements
     * @tparam S the type of the state used to generate elements
     * @param state the initial state
     * @param f the function that takes the current state and returns
     *          an Option containing the next element and next state, or None to terminate
     * @return a lazy list built from the seed value and the generator function
     */
    def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
      f(state) match
        case Some((a,ns)) => 
          a #:: unfold(ns)(f)
        case None =>
          LazyList.empty

/** Extractor for pattern matching on LazyList.
 * 
 * Enables pattern matching with the #:: operator.
 */
object #:: {
    /** Extracts the head and tail of a lazy list.
     * 
     * @tparam A the type of lazy list elements
     * @param s the lazy list to extract from
     * @return Some((head, tail)) if the list is non-empty, otherwise None
     */
    def unapply[A](s: LazyList[A]): Option[(A, LazyList[A])] =
        if !s.isEmpty then Some((s.head, s.tail)) else None
}
