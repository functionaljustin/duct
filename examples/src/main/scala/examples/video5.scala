// Functional Justin https://youtu.be/wNVQ75KM8-4
// Ep 5: Scala 3 Type lambdas, Functors and error handling
// Demo of either which allows us to do error handling in a 
// purely functional way
object Video5 extends App:
  val e1: Either[String, Int] = Right(10)
  val e2: Either[String, Int] = Left("I am an error")

  val e3 = e1.map(a => a + 1)
  val e4 = e2.map(a => a + 1)

  // Functors and error handling
  println(e3)
  println(e4)