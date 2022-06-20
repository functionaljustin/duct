import org.justinhj.duct.typeclasses.monoid.{given,*}

class MonoidSuite extends munit.FunSuite {
  // List monoid
  test("left and right identity of list monoid") {
    assertEquals(List(1,2,3), List(1,2,3).combine(listMonoid.zero))
    assertEquals(List(1,2,3), listMonoid.zero.combine(List(1,2,3)))
  }
  test("associativity of list monoid") {
    val a = List(1,2,3)
    val b = List(4,5)
    val c = List(6)
    val ab = a.combine(b)
    val bc = b.combine(c)
    val abAndc = ab.combine(c)
    val aAndbc = a.combine(bc)
    assertEquals(abAndc,aAndbc)
  }
  // String monoid
  test("left and right identity of string monoid") {
    assertEquals("left side", "left side".combine(stringMonoid.zero))
    assertEquals("right side", stringMonoid.zero.combine("right side"))
  }
  test("associativity of string monoid") {
    val a = "apple"
    val b = "banana"
    val c = "canary melon"
    val ab = a.combine(b)
    val bc = b.combine(c)
    val abAndc = ab.combine(c)
    val aAndbc = a.combine(bc)
    assertEquals(abAndc,aAndbc)
  }
}
