import org.justinhj.duct.typeclasses.monad.{given, _}
import org.justinhj.duct.typeclasses.numeric.{given, _}
import org.justinhj.duct.typeclasses.monoid.{given, _}
import org.justinhj.duct.datatypes.WriterT

// Functional Justin https://youtu.be/d4co05kJIhQ
// Ep 10: Tell me more! Add logging to a pure program with Applicative and WriterT in Scala 3
// I take advantage of applicative with WriterT to combine logs when evaluating an expression
// Whew!

object Video10 extends App:

  enum EvalError:
    case InvalidSymboName
    case SymbolNotFound
    case DivisionByZero

  type EvalResult[A] = Either[EvalError, A]
  type EvalResultW[A] = WriterT[EvalResult,List[String],A]

  def mapTell2[A,B,C,F[_],W](fa: WriterT[F,W,A], fb: WriterT[F,W,B], fabc: (A,B) => C, fabcw: (A,B,C) => W)
                            (using m: Monoid[W], f: Monad[F]): WriterT[F,W,C] = {
    val r = fa.wrapped.map2(fb.wrapped){
      case ((al,a),(bl,b)) =>
        val c = fabc(a,b)
        val w = fabcw(a,b,c)
        val prev = al.combine(bl)
        (prev.combine(w),c)
    }
    WriterT(r)
  }
  
  // Implement Numeric for EvalResult
  given evalResultNumeric[A: Numeric]: Numeric[EvalResultW[A]] with {

    def isZero(a: EvalResultW[A]): Boolean = {
      a.value match {
        case Right(a) if summon[Numeric[A]].isZero(a) => true
        case _ => false
      }
    }

    def add(fa: EvalResultW[A], fb: EvalResultW[A]): EvalResultW[A] = {
      mapTell2(fa,fb,(a,b) => a + b,(a,b,c) => List(s"$c: added $a to $b"))
    }

    def div(fa: EvalResultW[A], fb: EvalResultW[A]): EvalResultW[A] = {
      if isZero(fb) then
        WriterT.lift(Left(EvalError.DivisionByZero))
      else
        mapTell2(fa,fb,(a,b) => a / b,(a,b,c) => List(s"$c: divided $a by $b"))
    }

    def sub(fa: EvalResultW[A], fb: EvalResultW[A]): EvalResultW[A] = {
      mapTell2(fa,fb,(a,b) => a + b,(a,b,c) => List(s"$c: subtracted $a from $b"))
    }

    def mul(fa: EvalResultW[A], fb: EvalResultW[A]): EvalResultW[A] = {
      mapTell2(fa,fb,(a,b) => a + b,(a,b,c) => List(s"$c: multiplied $a by $b"))
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

  type WithEnv[A] = Env[A] ?=> EvalResultW[A]

  def summonEnv[A] : Env[A] ?=> Env[A] = summon[Env[A]]
  
  def eval[A : Numeric](exp: Exp[A]): WithEnv[A] =
    exp match
      case Var(id) => handleVar(id).tellWith(a => List(s"Handled var $id val $a"))
      case Val(value) => WriterT.lift[List[String]](Right(value))
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
      case Some(value) => WriterT.lift[List[String]](Right(value))
      case None => WriterT.lift[List[String]](Left(EvalError.SymbolNotFound))
    }

  val exp1 : Exp[Int] = Add(Var("z"), Add(Val(10), Add(Var("x"), Var("y"))))

  {
    // Test some operations
    given envMap: Env[Int] = Map("x" -> 7, "y" -> 6, "z" -> 22)
    val eval1 = eval(exp1)
    println(eval1)
  }

  {
    // Division by zero
    given envMap: Env[Int] = Map.empty
    val expO1 = Div(Val(10), Val(0))
    println(eval(expO1))
  }