package org.justinhj.duct.datatypes

import scala.annotation.tailrec

// A nonempty implementation of lazy list. No video for this one but it is used in the Comonad
// video since you can't make a full Comonad instance for lazy list. The implementation is a
// straightforward nonempty handling interface on top of LazyList

sealed trait NonEmptyLazyList[+A]:
    def head: A
    def tail: LazyList[A]

    def tailOption: Option[NonEmptyLazyList[A]] =
      if tail.isEmpty then None
      else Some(NonEmptyLazyList.cons(tail.head, tail.tail))

    def toLazyList: LazyList[A] =
      LazyList.cons(head, tail)

    def map[B](f: A => B): NonEmptyLazyList[B] =
      NonEmptyLazyList.cons(f(head), tail.map(f))

    @tailrec
    final def forEach(f: A => Unit): Unit =
      f(head)
      this.tailOption match
        case Some(tail) => tail.forEach(f)
        case None => ()

    def tails: NonEmptyLazyList[NonEmptyLazyList[A]] =
      NonEmptyLazyList.cons(this, 
        this.tailOption match
          case Some(tail) => 
            tail.tails.toLazyList
          case None => LazyList.empty)

    def zip[B](other: NonEmptyLazyList[B]): NonEmptyLazyList[(A,B)] =
      NonEmptyLazyList.cons((this.head,other.head), this.tail.zip(other.tail))

    def sum[B >: A](implicit num: Numeric[B]): B = this.toLazyList.sum

    def toList: List[A] = this.toLazyList.toList

    def take(n: Int): NonEmptyLazyList[A] =
      val ll = this.toLazyList.take(n)
      NonEmptyLazyList.cons(ll.head, ll.tail)        

object NonEmptyLazyList:
    def cons[A](hd: => A, tl: => LazyList[A]) = new NonEmptyLazyList[A]:
        lazy val head = hd
        lazy val tail = tl

    def apply[A](a: => A, as: A*): NonEmptyLazyList[A] =
        NonEmptyLazyList.cons(a, LazyList.apply(as: _*))

    def repeat[A](a: A): NonEmptyLazyList[A] = NonEmptyLazyList.cons(a, LazyList.repeat(a))
    def iterate[A](a: A)(next: A => A): NonEmptyLazyList[A] = NonEmptyLazyList.cons(a,LazyList.iterate(next(a))(next))