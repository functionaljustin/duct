package org.justinhj.duct.datatypes

case class NonEmptyList[A](head:A, tail:A*) {
  def append(right: NonEmptyList[A]): NonEmptyList[A] = {
    NonEmptyList(head, (tail :+ right.head) ++ right.tail:_*)
  }
  def toList: List[A] = (head +: tail).toList
}

object NonEmptyList:
  def fromSeq[A](items: Seq[A]): Option[NonEmptyList[A]] = {
    if items.size > 0 then
      Some(NonEmptyList(items.head, items.tail:_*))
    else
      None
  }
