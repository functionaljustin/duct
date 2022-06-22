package org.justinhj.duct.datatypes

case class NonEmptyList[+A](head:A, tail:A*):
  // List we're appending to must be type A or a super-type
  def append[B >: A](right: NonEmptyList[B]): NonEmptyList[B] =
    NonEmptyList(head, (tail :+ right.head) ++ right.tail:_*)

  def toList: List[A] = (head +: tail).toList

object NonEmptyList:
  def fromSeq[A](items: Seq[A]): Option[NonEmptyList[A]] =
    if items.size > 0 then
      Some(NonEmptyList(items.head, items.tail:_*))
    else
      None
