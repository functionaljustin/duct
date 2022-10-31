package examples

import org.justinhj.duct.datatypes.CoReader
import org.justinhj.duct.typeclasses.comonad.{given, _}

object CoReaderEval extends App:

  enum Exp[A]:
    case Val(extract: A) extends Exp[A]
    case Add(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Sub(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Mul(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Div(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Var(identifier: String) extends Exp[A]

  import Exp._
  type Env[A] = Map[String, A]

  // Here's one way of copying with the fact that evaluating a CoReader takes you from effect
  // land to naked extracts, so you need to somehow lift again. Here the user has to provide
  // a function that lifts with the environment, and its sad and dumb.
  def coReaderEvalMap[A : Numeric](expr: Exp[A])(f: Exp[A] => CoReader[Env[A], Exp[A]]): A =
    val coReader = f(expr)
    coReader.extract match
       case Val(extract) => extract
       case Var(id) => coReader.ask(id) // When you actually want the environment it is right there
       case Add(left,right) => coReaderEvalMap(left)(f) + coReaderEvalMap(right)(f)
       case Sub(left,right) => coReaderEvalMap(left)(f) - coReaderEvalMap(right)(f)
       case Mul(left,right) => coReaderEvalMap(left)(f) * coReaderEvalMap(right)(f)
       case Div(left,right) => coReaderEvalMap(left)(f) / coReaderEvalMap(right)(f)

  // This seems more sane but not really a great use of the CoReader
  def eval[A : Numeric](expr: CoReader[Env[A],Exp[A]]): A =
    expr.extract match
       case Val(extract) => extract
       case Var(id) => expr.ask(id) // When you actually want the environment it is right there
       case Add(left,right) => eval(CoReader(left,expr.ask)) + eval(CoReader(right,expr.ask))
       case Sub(left,right) => eval(CoReader(left,expr.ask)) - eval(CoReader(right,expr.ask))
       case Mul(left,right) => eval(CoReader(left,expr.ask)) * eval(CoReader(right,expr.ask))
       case Div(left,right) => eval(CoReader(left,expr.ask)) / eval(CoReader(right,expr.ask))

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
    val cr2 = cr.coflatMap(a => a.extract + 1).coflatMap(a => a.extract * 2)
    println(s"cr2 $cr2")

    val evalMap1 = eval(CoReader(exp1,envMap))
    println(s"coReaderEvalMap exp gives $evalMap1")
  }
