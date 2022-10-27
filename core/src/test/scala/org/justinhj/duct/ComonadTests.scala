package org.justinhj.duct

import org.justinhj.duct.datatypes.{NonEmptyList,CoReader}
import org.justinhj.duct.typeclasses.comonad.{given,_}
import java.nio.charset.CoderResult

class ComonadTestSuite extends munit.FunSuite {
  val cm = Comonad[NonEmptyList]
  test("NonEmptyList extract") {
    assertEquals(cm.extract(NonEmptyList(1)),1)
  }
  test("NonEmptyList coflatMap") {
    assertEquals(NonEmptyList(1,2,3).coflatMap(identity), NonEmptyList(
      NonEmptyList(1,2,3), NonEmptyList(2,3), NonEmptyList(3)
      ))
  }
  test("compose of coKleisli") {
    // Whilst there is no implementation of compose in my Comonad it's 
    // a more enlightening way to explore the Comonad in general, see:
    //    https://bartoszmilewski.com/2017/01/02/comonads/ 
    def compose[A,B,C,F[_]](left: F[A] => B, right: F[B] => C): F[A] => C = {
      ???
    }
  }
  test("coreader laws") {
    // https://hackage.haskell.org/package/comonad-5.0.8/docs/Control-Comonad.html
    val cr = CoReader("environment1","value1")
    val f: String => String = a => a + a
    // assertEquals(cr.coflatMap(f), cr.map(f)) TODO
    assertEquals(cr.coflatMap(identity), cr.duplicate)
    assertEquals(cr.coflatMap(a => f(a.extract)), cr.map(f))
  }
  test("coreader example") {
    // expression example
  }
}