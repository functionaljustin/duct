package examples

import scala.annotation.tailrec
import org.justinhj.duct.datatypes.{LazyList,#::}

// Examples using the Duct LazyList
// These examples were created when developing the code and examples for episode 17
// Watch the video for more info

object Video17Extras extends App:

  def tailWithSideEffect: LazyList[Int] = {
    println("getting empty LazyList")
    LazyList.empty
  }

  println("list1")
  val list1 = LazyList.cons(
    1,
    tailWithSideEffect
  ) // SHOULD not print anything, and also is ok

  list1.tail // this would eval the tail

  println("forEach list1")
  LazyList(1,2,3).forEach { a =>
    println(a)
  }

  // Constructor
  val list2: LazyList[Int] =
    (1 #:: 2 #:: 3 #:: 4 #:: 5 #:: LazyList.empty)
  list2.forEach { a =>
    println(a)
  }

  // take is lazy
  val ones = LazyList.repeat(1)
  ones.take(5).forEach { a =>
    println(a)
  }

  // zip
  val twos = LazyList.repeat(2)
  ones.zip(twos).take(5).forEach { a =>
    println(a)
  }

  // map
  val threes = LazyList.repeat(3)
  val mapped = threes.take(3).map(_ + 1).forEach { a =>
    println(a)
  }

  // test lazy head
  def evalMe(n: Int): Int = { println(s"evaluating $n"); n }

  val evalList = evalMe(1) #:: evalMe(2) #:: LazyList.empty
  println("ok lets eval first")
  evalList.head

  println("ok lets eval next")
  evalList.tail.head

  // matrix stuff

  // Zip two lazy lists with a map function that combines them to some new type
  def zipWith[A, B, C](as: LazyList[A], bs: LazyList[B])(
      f: (A, B) => C
  ): LazyList[C] = {
    as.zip(bs).map { case (a, b) => f(a, b) }
  }

  def incN(n: Int, inc: Int): LazyList[Int] =
    LazyList.cons(n, incN(n + inc, inc))

  // test zipWith and repeat (this is okay)
  val threeFactors = incN(3, 3)
  val zw1 = zipWith(ones, threeFactors) { case (a, b) => a * b }
  zw1.take(10).forEach {
    println(_)
  }

  // Nice example from Applicative programming with Effects paper, maybe a later video

  def transpose[A](
      matrix: LazyList[LazyList[A]]
  ): LazyList[LazyList[A]] = {
    if matrix.isEmpty then LazyList.repeat(LazyList.empty)
    else
      zipWith(matrix.head, transpose(matrix.tail))(_ #:: _)
      // { case (a, as) =>
      //   LazyList.cons(a, as)
      // }
  }

  val matrix = LazyList(
    LazyList(11, 12, 13, 14, 15),
    LazyList(21, 22, 23, 24, 25),
    LazyList(31, 32, 33, 34, 35)
  )

  matrix.forEach { l =>
    l.forEach { l2 =>
      print(f"$l2 ")
    }
    println()
  }

  val transposed = transpose(matrix)

  transposed.forEach { l =>
    l.forEach { l2 =>
      print(f"$l2 ")
    }
    println()
  }

  // the fold debacle

  // fold left and fold right, what is the difference and what is the stack safety?

  val l1 = List("a", "b", "c")

  println(l1.foldLeft("d") { (acc, a) =>
    {
      println(s"acc $acc a $a")
      acc + a
    }
  }) // "dabc"

  println(l1.foldRight("d") { (a, acc) =>
    {
      println(s"acc $acc a $a")
      acc + a
    }
  }) // "dcba"

  // with SBT_OPTS="-XX:+UseG1GC -Xmx4G" sbt
  // this does not stack overflow or OOM. nice.
  // 7 seconds to sum 10m bigint

  // println(
  //   incN(1, 1).take(10000000).foldLeft(BigInt(0)) { case (acc, a) => acc + a }
  // )


  // Stack overflow with foldRight
  // println(
  //   incN(1, 1).take(10000000).foldRight(BigInt(0)) { case (acc, a) => a + acc }
  // )

  // forEach works with large data sets too but is much slower (about 2 minutes)
  // var sum: BigInt = 0
  // incN(1,1).take(10000000).forEach { a =>
  //   sum += a
  // }
  // println(s"sum $sum")

  // unapply works (see definition above), for example
  def mapunapply[A, B](ll: LazyList[A], f: A => B): LazyList[B] = {
    ll match {
      case hd #:: tl =>
        LazyList.cons(f(hd), mapunapply(tl, f))
      case _ =>
        LazyList.empty
    }
  }

  mapunapply(ones.take(10), {_ + 1}).forEach { a =>
    println(s"a $a")
  }

  // dropWhile
  incN(1, 1).dropWhile(_ <= 10).take(5).forEach(println(_))

  // filter
  incN(1, 1).filter(_ % 2 == 0).take(10).forEach(println(_))

  // foldRight shows early exit of iteration
  println(incN(1,1).take(10).foldRight(0){
    (a,b) => {
        println(s"a $a")
        if a < 5 then
          a + b
        else
          b
    }
  })

  def hasTuna(ll: LazyList[String]): Boolean = {
    ll.foldRight(false){
      (next, z) => 
        println(next)
        if next == "tuna" then
          true
        else
          z
    }
  }

  def hasTunaStdLib(ll: scala.collection.immutable.LazyList[String]): Boolean = {
    ll.foldRight(false){
      (next, z) => 
        println(next)
        if next == "tuna" then
          true
        else
          z
    }
  }

  println("hastuna")
  hasTuna(LazyList("salmon", "shark", "tuna", "moray", "goldfish", "eel"))

  println("hastuna stdlib")
  hasTunaStdLib(scala.collection.immutable.LazyList("salmon", "shark", "tuna", "moray", "goldfish", "eel"))

  // fibs
  val fibs: LazyList[BigInt] =
    BigInt(0) #:: BigInt(1) #::
      fibs.zip(fibs.tail).map { (a, b) =>
        println(s"Adding $a and $b")
        a + b
      }

  fibs.take(10).forEach { a =>
    println(a)
  }

  fibs.take(11).forEach { a =>
    println(a)
  }

  // Finding things lazily

  println(LazyList("salmon", "shark", "moray").first(_ == "tuna"))
  println(LazyList("salmon", "shark", "tuna", "moray", "goldfish", "eel").first(_ == "tuna"))

  // Can do this with LazyList but not Scala lazy list
  // https://stackoverflow.com/questions/7830471/foldright-on-infinite-lazy-structure

  println("take 5 from infinite foldRight")
  LazyList.from(1).foldRight(LazyList.empty: LazyList[Int])( (i, s) => i #:: s).take(5).forEach(println(_))

  // LazyList.from(1).foldRight(LazyList.empty: LazyList[Int])( (i, s) => i #:: s).take(5).foreach(println(_))
  // println("also done") // never happens

  // http://voidmainargs.blogspot.com/2011/08/folding-stream-with-scala.html
  LazyList.repeat(true).foldRight(false){
    _ || _
  }

  // Partition (not very interesting)
  println("partition")
  val (l,r) = LazyList.from(1).partition(_ % 2 == 0)
  l.take(20).forEach(println(_))
  r.take(20).forEach(println(_))

  // headOption 
  val fish = LazyList("salmon", "shark", "tuna", "moray", "goldfish", "eel")

  // Make a tuple of fish names and length
  val nameAndLengths = fish.map { f =>
    (f, f.length)
  }
  // Find the longest fish name
  val longest = nameAndLengths.foldLeft(("", 0)) {
    case ((accName, accLength), (name, length)) =>
      if length > accLength then
        (name, length)
      else
        (accName, accLength)
  }
  println(s"The longest fish name is ${longest._1} with length ${longest._2}")
  
  println(
    fish.filter(_ == "tuna").
      headOption)

  // LazyList of primes
  def sieve(s: LazyList[Int]): LazyList[Int] =
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))

  val primes = sieve(LazyList.from(2)).take(10)
  primes.forEach(println(_))


