package org.justinhj.duct

import org.justinhj.duct.datatypes.{NonEmptyList,CoReader}
import org.justinhj.duct.typeclasses.comonad.{given,_}
import java.nio.charset.CoderResult

class ComonadTestSuite extends munit.FunSuite {
  val cm = Comonad[NonEmptyList]

  def compose[A,B,C](left: NonEmptyList[A] => B, right: NonEmptyList[B] => C): NonEmptyList[A] => C =
    fa =>
      val fb : NonEmptyList[B] = fa.coflatMap(left)
      val fc : NonEmptyList[C] = fb.coflatMap(right)
      fc.extract
      
  test("NonEmptyList extract") {
    assertEquals(cm.extract(NonEmptyList(1)),1)
  }
  test("NonEmptyList coflatMap") {
    assertEquals(NonEmptyList(1,2,3).coflatMap(identity), NonEmptyList(
      NonEmptyList(1,2,3), NonEmptyList(2,3), NonEmptyList(3)
      ))
  }
  test("NonEmptyList extract 2") {
    assertEquals(cm.extract(NonEmptyList(1)),1)
  }
  test("Compose") {
    // Whilst there is no implementation of compose in my Comonad it's 
    // a more enlightening way to explore the Comonad in general, see:
    //    https://bartoszmilewski.com/2017/01/02/comonads/ 
    val nel1 = NonEmptyList(1,2,3)
    val avg = nel1.foldLeft(0){case (acc, a: Int) => a + acc}
    assertEquals(avg,2)
    // val r = compose[Int,Int,Int](nel1 => nel1.foldLeft(0){case (acc, a: Int) => a + acc} / nel1.size, nel2 => nel2.foldLeft(0){case (acc, a: Int) => Math.max(a,acc)})
    // assertEquals(r(nel1),3)
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