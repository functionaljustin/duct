package org.functionaljustin.duct

import org.functionaljustin.duct.datatypes.{NonEmptyList,CoReader}
import org.functionaljustin.duct.typeclasses.comonad.{given,_}
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
  test("NonEmptyList left identity") {
    val input = NonEmptyList(2,3,4)
    assertEquals(input.coflatMap(cm.extract), input)
  }
  test("NonEmptyList right identity") {
    val input = NonEmptyList(2, 3, 4)
    val f: NonEmptyList[Int] => Int = nel => nel.head * 2 // Example function: doubles the head
    assertEquals(cm.extract(input.coflatMap(f)), f(input))
  }
  test("NonEmptyList associativity") {
    val input = NonEmptyList(2, 3, 4)
    val f: NonEmptyList[Int] => Int = nel => nel.head + 1 // Adds 1 to the head
    val g: NonEmptyList[Int] => Int = nel => nel.head * 2 // Doubles the head
    val left = input.coflatMap(f).coflatMap(g)
    val right = input.coflatMap(x => g(input.coflatMap(f)))
    assertEquals(left, right)
  }
  test("Compose") {
    // Whilst there is no implementation of compose in my Comonad it's 
    // a more enlightening way to explore the Comonad in general, see:
    //    https://bartoszmilewski.com/2017/01/02/comonads/ 
    val nel1 = NonEmptyList(1,2,3)
    val avg = nel1.foldLeft(0){
      case (acc, a: Int) => {
        // println(acc + " " + a);
        acc + a
      }
    }
    assertEquals(avg,1)
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
