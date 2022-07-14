package org.justinhj.duct.datatypes

case class LazyList[A](val head: Option[() => A], val tail: Option[LazyList[A]]) {
        lazy val first: A = head()
        lazy val rest = tail(first)
}
