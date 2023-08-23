package org.functionaljustin.duct.datatypes

// Ep 16: NonEmptyLists more or less
// https://youtu.be/7A2xuRkCZBg

// Note that I will do a future video on variance, so this is implemented as invariant for now. 
// If you don't know what that means, stay tuned!

case class NonEmptyList[A](head:A, tail:A*):
  // Append a NonEmptyList of type B which must be either the same
  // type as A or a super-type
  def append(right: NonEmptyList[A]): NonEmptyList[A] =
    NonEmptyList(head, (tail :+ right.head) ++ right.tail:_*)

  def toList: List[A] = (head +: tail).toList

object NonEmptyList:
  def fromSeq[A](items: Seq[A]): Option[NonEmptyList[A]] =
    if items.size > 0 then
      Some(NonEmptyList(items.head, items.tail:_*))
    else
      None
