package org.functionaljustin.duct.typeclasses.monad

import org.functionaljustin.duct.typeclasses.monad.{given,*}
import org.functionaljustin.duct.datatypes._

// TODO this could use MUnit's scalacheck integration for law testing
// https://scalameta.org/munit/blog/2020/03/24/scalacheck.html

// left identity
// forall a f. f(a) == bind(pure(a), f)
// right identity
// forall a. a == bind(a, x => pure(x))
// associativity
// forall a f g. bind(a, x => bind(f(x), g)) == bind(bind(a, f), g)
class MonadSuite extends munit.FunSuite {
  val nelM = Monad[NonEmptyList]
  val f: Int => NonEmptyList[Int] = a => NonEmptyList(a + 1)
  val g: Int => NonEmptyList[Int] = a => NonEmptyList(a * 2)
  test("nonEmptyList unit") {
    assertEquals(NonEmptyList(1), nelM.pure(1))
  }
  test("left identity") {
    assertEquals(f(1), nelM.pure(1).flatMap(f))
  }
  test("right identity") {
    assertEquals(NonEmptyList(1), NonEmptyList(1).flatMap(a => nelM.pure(a)))
  }
  test("associativity") {
    val nest1 = NonEmptyList(1).flatMap(a => f(a).flatMap(g))
    val nest2 = NonEmptyList(1).flatMap(f).flatMap(g)
    assertEquals(nest1, nest2)
  }
}
