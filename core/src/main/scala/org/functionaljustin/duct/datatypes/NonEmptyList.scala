package org.functionaljustin.duct.datatypes

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

// Ep 16: NonEmptyLists more or less
// https://youtu.be/7A2xuRkCZBg

// Note that I will do a future video on variance, so this is implemented as invariant for now. 
// If you don't know what that means, stay tuned!

case class NonEmptyList[A](head:A, tail:A*):
  // Append a NonEmptyList of type B which must be either the same
  // type as A or a super-type
  def append(right: NonEmptyList[A]): NonEmptyList[A] =
    NonEmptyList(head, (tail :+ right.head) ++ right.tail:_*)

  // TODO tests
  def tailUnsafe: NonEmptyList[A] = NonEmptyList(tail.head, tail.tail: _*)

  // TODO tests
  def tailOption: Option[NonEmptyList[A]] = {
    tail match {
      case _ :: tl =>
        Some(NonEmptyList(tl.head, tl.tail: _*))
      case _ => None  
    }
  }

  def toList: List[A] = (head +: tail).toList

  def map[B](f: A => B): NonEmptyList[B] = NonEmptyList(f(head), tail.map(f): _*)

  // TODO tests
  def forEach(f: A => Unit): Unit =  {
    f(head)
    if tail.nonEmpty then
      tail.foreach(f)
  }

  def size: Int = 1 + tails.size

  @tailrec
  final def foldLeft[B](z: B)(f: (B,A) => B): B =
    val next = f(z,head)
    tailOption match
      case Some(rest) => rest.foldLeft(next)(f)
      case None => next

  def tails: NonEmptyList[NonEmptyList[A]] = {
    var l = this.toList
    val tld = l.tails.filterNot(_.isEmpty).toList
    val nels = tld.map(n => NonEmptyList(n.head, n.tail: _*))
    NonEmptyList(nels.head, nels.tail: _*)
  }

  def reverse: NonEmptyList[A] = 
    val list = this.toList.reverse
    NonEmptyList(list.head, list.tail:_*)

object NonEmptyList:
  def fromSeq[A](items: Seq[A]): Option[NonEmptyList[A]] =
    if items.size > 0 then
      Some(NonEmptyList(items.head, items.tail:_*))
    else
      None 
