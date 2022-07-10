import org.justinhj.duct.typeclasses.functor.{given,*}
import org.justinhj.duct.datatypes._

class FunctorSuite extends munit.FunSuite {
  test("NonEmptyList map") {
    val input = NonEmptyList(1,2,3)
    val expected = NonEmptyList(2,3,4)
    assertEquals(input.map(a => a + 1), expected)
  }
}
