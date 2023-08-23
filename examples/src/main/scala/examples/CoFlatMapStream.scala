package examples

import org.functionaljustin.duct.datatypes.NonEmptyLazyList
import org.functionaljustin.duct.typeclasses.comonad.{given, _}

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

  // Design to illustrate what the stream processing is doing...

  val nums = NonEmptyLazyList.iterate(1)(_ + 1)
  val result = nums.take(5).toList
  println(s"take 5 from nums ${result}")

  // Now coflatMap it to print a rolling average of 10 elements
  val result2 = nums.coflatMap {
    nel =>
      val elements = nel.take(10).toList
      elements.sum / 10.0
  }.take(20).toList

  val sum1 = List(1,2,3,4,5,6,7,8,9,10).sum / 10.0
  println(sum1)

  val sum2 = List(2,3,4,5,6,7,8,9,10,11).sum / 10.0
  println(sum2)

  println(s"take 20 from nums ${result2}")

  // What's the behaviour of running out of elements?
  val fiveThings = NonEmptyLazyList(1,2,3,4,5)
  val result3 = fiveThings.take(10)
  println(s"result3 $result3")
