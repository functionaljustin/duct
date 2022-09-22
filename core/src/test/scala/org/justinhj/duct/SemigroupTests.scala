package org.justinhj.duct.typeclasses.semigroup

import org.justinhj.duct.typeclasses.semigroup.{given,*}
import org.justinhj.duct.datatypes.NonEmptyList

class SemigroupSuite extends munit.FunSuite {
  test("associativity of list semigroup") {
    val a = List(1,2,3)
    val b = List(4,5)
    val c = List(6)
    val ab = a.combine(b)
    val bc = b.combine(c)
    val abAndc = ab.combine(c)
    val aAndbc = a.combine(bc)
    assertEquals(abAndc,aAndbc)
  }
  test("associativity of string semigroup") {
    val a = "apple"
    val b = "banana"
    val c = "canary melon"
    val ab = a.combine(b)
    val bc = b.combine(c)
    val abAndc = ab.combine(c)
    val aAndbc = a.combine(bc)
    assertEquals(abAndc,aAndbc)
  }
  test("|+| operator for string") {
    assertEquals("This test passes", "This test " |+| "passes")
  }
  test("associativity of semigroup for nonemptylist") {
    val nel1 = NonEmptyList("Ape", "Boa", "Cat")
    val nel2 = NonEmptyList("Dog", "Elephant")
    assertEquals((nel1 |+| nel2).toList, List("Ape", "Boa", "Cat", "Dog", "Elephant"))
  }
}
