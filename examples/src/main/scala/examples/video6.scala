package examples

// Functional Justin https://youtu.be/3GPXEzO14ZE
// Ep 6: Functional error handling with applicative in Scala 3
// Here I covered the functor laws and implementing evaluation
// with applicative and functor for error handling with the 
// Either data type.
import org.justinhj.duct.typeclasses.monad.{given, _}
import org.justinhj.duct.typeclasses.numeric.{given, _}

object Video6 extends App:
  // Identity law for functors. TODO this can be a test
  val l1 = List(1, 2, 3)
  print("Functor identity law is true for List? ")
  println(l1 == l1.map(a => identity(a)))

  print("Functor identity law is true for Either? ")
  val e1: Either[String, Int] = Right(10)
  println(e1 == e1.map(identity))

  def f(a: Int): Int = a + 1
  def g(a: Int): Int = a - 1

  print("Functor preserves identity of morphisms for List? ")
  println(l1.map(f).map(g) == l1.map(a => g(f(a))))
  print("Functor preserves identity of morphisms for List? ")
  println(e1.map(f).map(g) == e1.map(a => g(f(a))))

  enum EvalError:
    case InvalidSymboName
    case SymbolNotFound
    case DivisionByZero

  type EvalResult[A] = Either[EvalError, A]

  // Implement Numeric for EvalResult
  given evalResultNumeric[A: Numeric]: Numeric[Either[EvalError, A]] with {

    def isZero(a: EvalResult[A]): Boolean = {
      a match {
        case Right(a) if summon[Numeric[A]].isZero(a) => true
        case _ => false
      }
    }

    def add(fa: EvalResult[A], fb: EvalResult[A]): EvalResult[A] = {
      fa.map2(fb)((a,b) => a + b)
    }

    def div(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {
      if isZero(b) then
        Left(EvalError.DivisionByZero)
      else
        a.map2(b)(_ / _)
    }

    def sub(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {

      a.map2(b)((a, b) => a - b)
    }

    def mul(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {
      // Note this could also use map2 but I use flatMap to demonstrate
      // how much extra work you have to do compared to using applicative's
      // map2 method...
      a.flatMap {
        aa =>
          b.map {
            bb =>
              aa * bb
          }
      }
    }
  }

  enum Exp[A]:
    case Val(value: A) extends Exp[A]
    case Add(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Sub(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Mul(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Div(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Var(identifier: String) extends Exp[A]

  type Env[A] = Map[String, A]

  import Exp._

  type WithEnv[A] = Env[A] ?=> Either[EvalError, A]

  def summonEnv[A] : Env[A] ?=> Env[A] = summon[Env[A]]

  def eval[A : Numeric](exp: Exp[A]): WithEnv[A] =
    exp match
      case Var(id) => handleVar(id)
      case Val(value) => Right(value)
      case Add(l,r) => handleAdd(l,r)
      case Sub(l,r) => handleSub(l,r)
      case Div(l,r) => handleDiv(l,r)
      case Mul(l,r) => handleMul(l,r)

  def handleAdd[A : Numeric](l: Exp[A] , r: Exp[A] ): WithEnv[A] = eval(l) + eval(r)
  def handleSub[A : Numeric](l: Exp[A] , r: Exp[A] ): WithEnv[A] = eval(l) - eval(r)
  def handleMul[A : Numeric](l: Exp[A] , r: Exp[A] ): WithEnv[A] = eval(l) * eval(r)
  def handleDiv[A : Numeric](l: Exp[A] , r: Exp[A] ): WithEnv[A] = eval(l) / eval(r)

  def handleVar[A](s: String): WithEnv[A] =
    summonEnv.get(s) match {
      case Some(value) => Right(value)
      case None => Left(EvalError.SymbolNotFound)
    }

  val exp1 : Exp[Int] = Mul(Var("z"), Add(Val(10), Sub(Var("x"), Var("y")))) 

  // Provide an environment and eval the expression
  {
    given envMap: Env[Int] = Map("x" -> 7, "y" -> 6, "z" -> 22)

    val eval1 = eval(exp1)

    println(s"Eval exp gives $eval1")
  }

  // And again with a different environment and a missing symbol
  {
    given envMap: Env[Int] = Map("x" -> 17, "y" -> 10, "a" -> 2)

    val eval1 = eval(exp1)

    println(s"Eval exp gives $eval1")
  }

  {
    // Test some operations
    given envMap: Env[Int] = Map("x" -> 1, "y" -> 10, "z" -> 100)
    val expO1 = Mul(Val(10), Var("y"))
    assert(eval(expO1) == Right(100))

    val expO2 = Div(Val(1000), Var("z"))
    assert(eval(expO2) == Right(10))

    val expO3 = Sub(Val(1000), Mul(Var("y"), Var("z")))
    assert(eval(expO3) == Right(0))
  }

  {
    // Division by zero
    given envMap: Env[Int] = Map.empty
    val expO1 = Div(Val(10), Val(0))
    assert(eval(expO1) == Left(EvalError.DivisionByZero))
  }