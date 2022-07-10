import org.justinhj.duct.typeclasses.monad.{given,*}
import org.justinhj.duct.datatypes._
// left identity
// forall a f. f(a) == bind(pure(a), f)
// right identity
// forall a. a == bind(a, x => pure(x))
// associativity
// forall a f g. bind(a, x => bind(f(x), g)) == bind(bind(a, f), g)
class MonadSuite extends munit.FunSuite {
  test("nonEmptyList unit") {
    assertEquals(NonEmptyList(1), nonEmptyListMonad.pure(1))
  }
  test("left identity") {
    val f: Int => NonEmptyList[Int] = a => NonEmptyList(a + 1)
    assertEquals(f(1), nonEmptyListMonad.pure(1).flatMap(f))
  }
  test("right identity") {
    assertEquals(NonEmptyList(1), NonEmptyList(1).flatMap(a => nonEmptyListMonad.pure(a)))
  }
}
