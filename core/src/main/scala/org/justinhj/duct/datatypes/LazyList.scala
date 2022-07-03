package org.justinhj.duct.datatypes

import scala.collection.immutable.{LazyList => NoThankyou}

final class LazyList2[A](head: => A, tail: Option[LazyList2[A]]) {
  def map[B](f: A => B): LazyList2[B] = {
    LazyList2(f(head), tail)
  }
}
