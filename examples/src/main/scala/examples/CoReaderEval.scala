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

  // Eval with CoReader, seems kind of trivial and pointless :(
  def eval[A : Numeric](cr: CoReader[Env[A], Exp[A]]): A =
    cr.value match
      case Val(value) => value
      case Var(id) => cr.ask(id)
      case Add(left,right) => eval(CoReader(left,cr.ask)) + eval(CoReader(right,cr.ask))
      case Sub(left,right) => eval(CoReader(left,cr.ask)) - eval(CoReader(right,cr.ask))
      case Div(left,right) => eval(CoReader(left,cr.ask)) / eval(CoReader(right,cr.ask))
      case Mul(left,right) => eval(CoReader(left,cr.ask)) * eval(CoReader(right,cr.ask))

  val x = CoReader(Map("x" -> 7, "y" -> 6, "z" -> 22), 0)


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

    val eval1 = eval(CoReader(exp1,envMap))
    println(s"Eval exp gives $eval1")
  }
