package examples

import org.justinhj.duct.datatypes.NonEmptyLazyList
import org.justinhj.duct.typeclasses.comonad.{given, _}

object CoFlatMapStream extends App:

  // This can all migrate to tests
  // val nell = NonEmptyLazyList(1,2,3)
  // println(s"head ${nell.head}")
  // println(s"tailOption ${nell.tailOption}")
  // nell.map(_ + 1).forEach(println)

  // val nell2 = NonEmptyLazyList(1,2)
  // println(s"tailOption ${nell2.tailOption}")

  // nell.tails.forEach{nel => println("nel"); nel.forEach(println)}

  // val nel3 = NonEmptyLazyList("one","two","three","four")
  // nel3.zip(nell).forEach(println)

  // Process a pure infinite stream with coFlatMap
  
  def linearFilter(weights: NonEmptyLazyList[Double])(s: NonEmptyLazyList[Double]): Double =
    weights.zip(s).map{case (a,b) => a*b}.sum

  val s1 = NonEmptyLazyList.iterate(1.0)(x => x + 1.0)

  println(linearFilter(NonEmptyLazyList(10,20,30,20,10))(s1))

  println(
    s1.coflatMap(linearFilter(NonEmptyLazyList(0.25,0.5,0.25))).take(50).toList
  )