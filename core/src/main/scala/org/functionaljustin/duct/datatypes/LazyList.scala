package org.functionaljustin.duct.datatypes

import scala.annotation.tailrec
import math.Numeric.Implicits.infixNumericOps

// Code developed in the video https://youtu.be/laB15gG5bjY Ep 17: The Magic of LazyLists
// This is a simple as possible implementation of LazyList based roughly on the original 
// Scala stdlib version from 2003 by Odersky.

sealed trait LazyList[+A] {
    def head: A
    def tail: LazyList[A]
    def isEmpty: Boolean

    def headOption = if !isEmpty then     
        Some(head) 
      else 
        None

    def tailOption = if isEmpty then None
        else 
            if tail.isEmpty then None
            else Some(tail)

    def map[B](f: A => B): LazyList[B] =
        if isEmpty then LazyList.empty
        else LazyList.cons(f(head), tail.map(f))

    def tails: LazyList[LazyList[A]] =
      if isEmpty then LazyList.empty
      else LazyList.cons(this, tail.tails)

    def coflatMap[B](f: LazyList[A] => B): LazyList[B] = {
      this.tails.map(f)
    }

    def zip[B](other: LazyList[B]): LazyList[(A,B)] = {
        if isEmpty || other.isEmpty then LazyList.empty
        else LazyList.cons((head, other.head), tail.zip(other.tail))
    }

    @tailrec
    final def dropWhile(f: A => Boolean): LazyList[A] = {
        if isEmpty then LazyList.empty
        else if f(head) then tail.dropWhile(f)
        else this
    }

    def filter(f: A => Boolean): LazyList[A] = {
        val dropped = this.dropWhile(a => !f(a))
        if dropped.isEmpty then LazyList.empty
        else LazyList.cons(dropped.head, dropped.tail.filter(f))    
    }    

    @tailrec
    final def drop(n: Int): LazyList[A] = {
        if n == 0 || isEmpty then this
        else tail.drop(n-1)
    }

    final def take(n: Int): LazyList[A] = {
        if n == 0 || isEmpty then LazyList.empty
        else LazyList.cons(head, tail.take(n-1))
    } 

    def exists(f: A => Boolean): Boolean = {
        this.first(f).isDefined
    }

    @tailrec    
    final def forall(f: A => Boolean): Boolean = {
        if isEmpty then true
        else f(head) && tail.forall(f)
    }

    def toList: List[A] = {
        if isEmpty then List.empty[A]
        else head +: tail.toList
    }

    @tailrec
    final def forEach(f: A => Unit): Unit = {
        if !isEmpty then
            f(head)
            tail.forEach(f)
    }

    @tailrec
    final def foldLeft[B](z: B)(f: (B, A) => B): B = {
        if isEmpty then z
        else tail.foldLeft(f(z, head))(f)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
        if isEmpty then z
        else
            f(head, tail.foldRight(z)(f))
    }

    def partition(f: A => Boolean): (LazyList[A],LazyList[A]) = {
      (filter(f), filter(a => !f(a)))
    }

    def first(f: A => Boolean) = this.filter(f).headOption

    def sum[B >: A](implicit num: Numeric[B]): B =
        this.foldLeft(Numeric[B].zero) {
            (acc,n) =>
                acc + n
        }

    def ++[BB >: A](e: => LazyList[BB]): LazyList[BB] =
        foldRight(e){
            case (hd,acc) =>
                LazyList.cons(hd, acc)
        }
}

object LazyList:
    val empty = new LazyList[Nothing] {
        def head = throw new NoSuchElementException("Cannot get head of empty lazy list")
        def tail = throw new UnsupportedOperationException("No tail of empty lazy list")
        val isEmpty = true
    }

    // Smart constructor for lazy lists
    def cons[A](hd: => A, tl: => LazyList[A]) = new LazyList[A] {
        lazy val head = hd
        lazy val tail = tl
        val isEmpty = false
    }    

    // Build a lazy list from a variable argument list
    def apply[A](as: A*): LazyList[A] = {
        if as.isEmpty then LazyList.empty
        else LazyList.cons(as.head, apply(as.tail*))
    }    

    extension [A](hd: => A)
        def #::(tl: => LazyList[A]): LazyList[A] = 
            LazyList.cons(hd, tl)

    // Repeat a thing forever; an infinite list generator...
    def repeat[A](a: A): LazyList[A] = a #:: repeat(a)
    def iterate[A](a: A)(next: A => A): LazyList[A] = a #:: iterate(next(a))(next)

    // All the integers forever (until Int.MaxValue anyway)
    def from(n: Int) : LazyList[Int] = n #:: from(n+1)

    def range(begin: Int, end: Int): LazyList[Int] = {
        def helper(n: Int, end: Int): LazyList[Int] = {
            if n == end then LazyList.empty
            else LazyList.cons(n, helper(n + 1, end))
        }
        helper(begin,end)
    }

    def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
      f(state) match
        case Some((a,ns)) => 
          a #:: unfold(ns)(f)
        case None =>
          LazyList.empty

object #:: {
    def unapply[A](s: LazyList[A]): Option[(A, LazyList[A])] =
        if !s.isEmpty then Some((s.head, s.tail)) else None
}
