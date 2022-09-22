package org.justinhj.duct

import org.justinhj.duct.datatypes.NonEmptyList
import org.justinhj.duct.typeclasses.comonad.{given,_}

class ComonadTestSuite extends munit.FunSuite {
  val cm = Comonad[NonEmptyList]
  test("nonEmptyList extract") {
    assertEquals(cm.extract(NonEmptyList(1)),1)
  }
  test("nonEmptyList coflatMap") {
    assertEquals(NonEmptyList(1,2,3).coflatMap(identity), NonEmptyList(
      NonEmptyList(1,2,3), NonEmptyList(2,3), NonEmptyList(3)
      ))
  }
}