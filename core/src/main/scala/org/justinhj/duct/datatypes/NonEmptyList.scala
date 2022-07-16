package org.justinhj.duct.datatypes

object NonEmptyList:
    def fromSeq[A](s: Seq[A]): Option[NonEmptyList[A]] = {
        s match {
            case first :: rest => Some(NonEmptyList(first, rest: _*))
            case Nil => None

        }
    }

final case class NonEmptyList[A](first: A, rest: A*) {
    def append(other: NonEmptyList[A]): NonEmptyList[A] = {
        NonEmptyList(first,
            (rest :+ other.first) ++ other.rest : _*
            )
    }

    def toList(): List[A] = {
        (first +: rest).toList
    }
}
