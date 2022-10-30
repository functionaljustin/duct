package examples

import org.justinhj.duct.datatypes.CoReader
import org.justinhj.duct.typeclasses.comonad.Comonad

object CoReaderEval extends App:

  enum Exp[A]:
    case Val(value: A) extends Exp[A]
    case Add(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Sub(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Mul(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Div(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Var(identifier: String) extends Exp[A]

  import Exp._
  type Env[A] = Map[String, A]

  // Here's one way of copying with the fact that evaluating a CoReader takes you from effect
  // land to naked values, so you need to somehow lift again. Here the user has to provide
  // a function that lifts with the environment, and its sad and dumb.
  def coReaderEvalMap[A : Numeric](expr: Exp[A])(f: Exp[A] => CoReader[Env[A], Exp[A]]): A =
    val coReader = f(expr)
    coReader.value match
       case Val(value) => value
       case Var(id) => coReader.ask(id) // When you actually want the environment it is right there
       case Add(left,right) => coReaderEvalMap(left)(f) + coReaderEvalMap(right)(f)
       case Sub(left,right) => coReaderEvalMap(left)(f) - coReaderEvalMap(right)(f)
       case Mul(left,right) => coReaderEvalMap(left)(f) * coReaderEvalMap(right)(f)
       case Div(left,right) => coReaderEvalMap(left)(f) / coReaderEvalMap(right)(f)

  // A sample expression
  val exp1 : Exp[Int] = Add(
                            Var("z"),
                            Add(
                              Val(10),
                              Mul(
                                Var("x"),
                                Var("y"))))

   {
    val envMap: Env[Int] = Map("x" -> 7, "y" -> 6, "z" -> 22)

    // Can map or coflatMap help? What does it mean to coflatMap a Coreader
    val cr = CoReader(10,envMap)  
    val cr2 = cr.coflatMap(a => a.value + 1).coflatMap(a => a.value * 2)
    println(s"cr2 $cr2")

    // What about applicative? Add = + e1 e2
    // remember applicative map 2 can be implemented with flatmap and map
    // assuming that we can do the same here

    val cr3 = CoReader(10,envMap)
    val cr4 = CoReader(15,envMap)

    // This is not finished but it doesn't work. Take a look at sat ap for Reader to see how to 
    // solve that although we can isolate the values A and B to call f, the function f has no way
    // to access the environment
    // def map2[A,B,C,E](e1: CoReader[E,A], e2: CoReader[E,B])(f: (A,B) => C): CoReader[E,C] =
    //   val fa = e1.coflatMap {
    //     cr1 => (b: B) => f(cr1.value,b)
    //   }
    //   e2.coflatMap {
    //     cr2 => cr2.value
    //   }


    val evalMap1 = coReaderEvalMap(exp1)(a => CoReader(a,envMap))
    println(s"coReaderEvalMap exp gives $evalMap1")
  }
