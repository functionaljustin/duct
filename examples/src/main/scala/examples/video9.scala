package examples

// Functional Justin https://youtu.be/KMm71HLoy0w
// Ep 9: Transformers, Monads in Disguise
import org.justinhj.duct.typeclasses.monad.{given,_}
import org.justinhj.duct.typeclasses.monoid.{given,_}
import org.justinhj.duct.typeclasses.functor.{given,_}
import org.justinhj.duct.datatypes.WriterT

object Video9 extends App:
  val e1 : Either[String, Int] = Right(10)
  val we1 = WriterT.lift[List[String]](e1)
  
  type StringEither[A] = Either[String,A]
  type StringEitherWriter[A] = WriterT[StringEither,List[String],A]
  
  def incrementEven(n: Int): StringEither[Int] =
    if n % 2 == 0 then Right(n+1) else Left("Not an even number")
    
  def doubleOdd(n: Int): StringEither[Int] =
    if n % 2 == 1 then Right(n *2) else Left("Not an odd number")
    
  val program1: StringEitherWriter[Int] = for (
    a <- WriterT.pure[StringEither,List[String]](10).tellWith(a => List(s"Initialized with $a"));
    b <- WriterT.lift[List[String]](incrementEven(a)).tellWith(a => List(s"incremented to $a"));
    c <- WriterT.lift[List[String]](doubleOdd(b)).tellWith(a => List(s"doubled to $a"))
  ) yield c
  
  println(s"program1 $program1")