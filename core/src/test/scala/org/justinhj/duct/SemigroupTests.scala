import org.justinhj.duct.typeclasses.semigroup.{given,*}

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
}
