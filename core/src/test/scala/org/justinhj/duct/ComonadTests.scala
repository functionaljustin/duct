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
  test("compose of coKleisli") {
    // Whilst there is no implementation of compose in my Comonad it's 
    // a more enlightening way to explore the Comonad in general, see:
    //    https://bartoszmilewski.com/2017/01/02/comonads/ 
    def compose[A,B,C,F[_]](left: F[A] => B, right: F[B] => C): F[A] => C = {
      ???
    }
  }
}