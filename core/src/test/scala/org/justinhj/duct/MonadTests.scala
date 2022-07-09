import org.justinhj.duct.typeclasses.monad.{given,*}
import org.justinhj.duct.datatypes._

class MonadSuite extends munit.Funsuite {
  test("nonEmptyList unit") {
    assertEquals(NonEmptyList(1), nonEmptyListMonad.pure(1))
  }
}
