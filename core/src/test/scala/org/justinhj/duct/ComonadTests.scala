package org.justinhj.duct

import org.justinhj.duct.datatypes.NonEmptyList
import org.justinhj.duct.typeclasses.Comonad
import org.justinhj.duct.typeclasses.nonEmptyListComonad

class ComonadTestSuite extends munit.FunSuite {
  val cm = Comonad[NonEmptyList]
  test("nonEmptyList extract") {
    assertEquals(cm.extract(NonEmptyList(1)),1)
  }
}
