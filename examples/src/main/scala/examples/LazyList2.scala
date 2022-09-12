package examples

import scala.annotation.tailrec
import org.justinhj.duct.datatypes.NonEmptyList

object LazyList2 extends App:

  trait OurLazyList[+A] {
    def head: A
    def tail: OurLazyList[A]

    def isEmpty: Boolean

    def headOption = if !isEmpty then     
        Some(head) 
      else 
        None

    @tailrec
    final def forEach(f: A => Unit): Unit = {
      if !isEmpty then
        f(head)
        tail.forEach(f)
    }

    def take(n: Int): OurLazyList[A] = {
      if n == 0 || isEmpty then OurLazyList.empty
      else OurLazyList.cons(head, tail.take(n - 1))
    }

    def zip[B](other: OurLazyList[B]): OurLazyList[(A, B)] = {
      if isEmpty || other.isEmpty then OurLazyList.empty
      else OurLazyList.cons((head, other.head), tail.zip(other.tail))
    }

    def map[B](f: A => B): OurLazyList[B] =
      if isEmpty then OurLazyList.empty
      else OurLazyList.cons(f(head), tail.map(f))

    def dropWhile(f: A => Boolean): OurLazyList[A] =
      if isEmpty then OurLazyList.empty
      else if f(head) then tail.dropWhile(f)
      else this

    def filter(f: A => Boolean): OurLazyList[A] =
      val dropped = this.dropWhile(a => !f(a))
      if dropped.isEmpty then OurLazyList.empty
      else OurLazyList.cons(dropped.head, dropped.tail.filter(f))

    @tailrec
    final def foldLeft[B](z: B)(f: (B, A) => B): B = {
      if isEmpty then z
      else tail.foldLeft(f(z, head))(f)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
       if isEmpty then z
       else
         f(head, tail.foldRight(z)(f))
    }

    // Another way to do this is this.filter(f).headOption
    def first(f: A => Boolean) : Option[A] = {
      if isEmpty then None
      else foldRight(None: Option[A]) {
        (a, b) =>
          println(s"observe $a")
          if f(a) then
            Some(a)
          else
            b
      }
    }

    def partition(f: A => Boolean): (OurLazyList[A],OurLazyList[A]) = {
      (filter(f), filter(a => !f(a)))
    }
  }

  object OurLazyList:
    val empty = new OurLazyList[Nothing] {
      def head = throw new NoSuchElementException("No head of empty")
      def tail = throw new UnsupportedOperationException("No tail of empty")
      def isEmpty = true
    }

    def cons[A](hd: => A, tl: => OurLazyList[A]) = new OurLazyList[A] {
      lazy val head = hd
      lazy val tail = tl

      def isEmpty = false
    }

    def apply[A](as: A*): OurLazyList[A] = {
      if as.isEmpty then OurLazyList.empty
      else cons(as.head, apply(as.tail: _*))
    }

    def repeat[A](a: A): OurLazyList[A] = a #:: repeat(a)

    def from(n: Int) : OurLazyList[Int] = n #:: from(n+1)

    // Note: right associative extension methods need to swap the parameters
    // see https://docs.scala-lang.org/scala3/reference/contextual/right-associative-extension-methods.html
    extension [A](hd: => A)
      def #::(tl: => OurLazyList[A]): OurLazyList[A] =
        OurLazyList.cons(hd, tl)

  // Note: without extension methods this would have been written:
  //  class Deferrer[A](tl: => OurLazyList[A]) {
  //    def #::(hd: A): OurLazyList[A] =
  //      OurLazyList.cons(hd, tl)
  //  }
  //
  //  implicit def toDeferrer[A](l: => OurLazyList[A]): Deferrer[A] =
  //    new Deferrer[A](l)

  object #:: {
    def unapply[A](s: OurLazyList[A]): Option[(A, OurLazyList[A])] =
      if (!s.isEmpty) Some((s.head, s.tail)) else None
  }

  // Example code

  def tailWithSideEffect: OurLazyList[Int] = {
    println("getting empty OurLazyList")
    OurLazyList.empty
  }

  println("list1")
  val list1 = OurLazyList.cons(
    1,
    tailWithSideEffect
  ) // SHOULD not print anything, and also is ok

  list1.tail // this would eval the tail

  println("forEach list1")
  list1.forEach { a =>
    println(a)
  }

  // Constructor
  val list2: OurLazyList[Int] =
    (1 #:: 2 #:: 3 #:: 4 #:: 5 #:: OurLazyList.empty)
  list2.forEach { a =>
    println(a)
  }

  // take is lazy
  val ones = OurLazyList.repeat(1)
  ones.take(5).forEach { a =>
    println(a)
  }

  // zip
  val twos = OurLazyList.repeat(2)
  ones.zip(twos).take(5).forEach { a =>
    println(a)
  }

  // map
  val threes = OurLazyList.repeat(3)
  val mapped = threes.take(3).map(_ + 1).forEach { a =>
    println(a)
  }

  // test lazy head
  def evalMe(n: Int): Int = { println(s"evaluating $n"); n }

  val evalList = evalMe(1) #:: evalMe(2) #:: OurLazyList.empty
  println("ok lets eval first")
  evalList.head

  println("ok lets eval next")
  evalList.tail.head

  // matrix stuff

  // Zip two lazy lists with a map function that combines them to some new type
  def zipWith[A, B, C](as: OurLazyList[A], bs: OurLazyList[B])(
      f: (A, B) => C
  ): OurLazyList[C] = {
    as.zip(bs).map { case (a, b) => f(a, b) }
  }

  def incN(n: Int, inc: Int): OurLazyList[Int] =
    OurLazyList.cons(n, incN(n + inc, inc))

  // test zipWith and repeat (this is okay)
  val threeFactors = incN(3, 3)
  val zw1 = zipWith(ones, threeFactors) { case (a, b) => a * b }
  zw1.take(10).forEach {
    println(_)
  }

  def transpose[A](
      matrix: OurLazyList[OurLazyList[A]]
  ): OurLazyList[OurLazyList[A]] = {
    if matrix.isEmpty then OurLazyList.repeat(OurLazyList.empty)
    else
      zipWith(matrix.head, transpose(matrix.tail)) { case (a, as) =>
        OurLazyList.cons(a, as)
      }
  }

  val matrix = OurLazyList(
    OurLazyList(11, 12, 13, 14, 15),
    OurLazyList(21, 22, 23, 24, 25),
    OurLazyList(31, 32, 33, 34, 35)
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

  println(
    incN(1, 1).take(10000000).foldLeft(BigInt(0)) { case (acc, a) => acc + a }
  )

  // forEach works with large data sets too but is much slower (about 2 minutes)
  // var sum: BigInt = 0
  // incN(1,1).take(10000000).forEach { a =>
  //   sum += a
  // }
  // println(s"sum $sum")

  // unapply works (see definition above), for example
  def mapunapply[A, B](ll: OurLazyList[A], f: A => B): OurLazyList[B] = {
    ll match {
      case hd #:: tl =>
        OurLazyList.cons(f(hd), mapunapply(tl, f))
      case OurLazyList.empty =>
        OurLazyList.empty
    }
  }

  // mapunapply(ones.take(10), {_ + 1}).forEach { a =>
  //   println(s"a $a")
  // }

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

  def hasTuna(ll: OurLazyList[String]): Boolean = {
    ll.foldRight(false){
      (next, z) => 
        println(next)
        if next == "tuna" then
          true
        else
          z
    }
  }

  def hasTunaStdLib(ll: LazyList[String]): Boolean = {
    ll.foldRight(false){
      (next, z) => 
        println(next)
        if next == "tuna" then
          true
        else
          z
    }
  }

  hasTuna(OurLazyList("salmon", "shark", "moray"))

  hasTunaStdLib(LazyList("salmon", "shark", "tuna", "moray", "goldfish", "eel"))

  // fibs
  val fibs: OurLazyList[BigInt] =
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

  println(OurLazyList("salmon", "shark", "moray").first(_ == "tuna"))
  println(OurLazyList("salmon", "shark", "tuna", "moray", "goldfish", "eel").first(_ == "tuna"))

  // Can do this with OurLazyList but not Scala lazy list
  // https://stackoverflow.com/questions/7830471/foldright-on-infinite-lazy-structure

  println("take 5 from infinite foldRight")
  OurLazyList.from(1).foldRight(OurLazyList.empty: OurLazyList[Int])( (i, s) => i #:: s).take(5).forEach(println(_))

  // LazyList.from(1).foldRight(LazyList.empty: LazyList[Int])( (i, s) => i #:: s).take(5).foreach(println(_))
  // println("also done") // never happens

  // http://voidmainargs.blogspot.com/2011/08/folding-stream-with-scala.html
  OurLazyList.repeat(true).foldRight(false){
    _ || _
  }

  // Partition (not very interesting)
  println("partition")
  val (l,r) = OurLazyList.from(1).partition(_ % 2 == 0)
  l.take(20).forEach(println(_))
  r.take(20).forEach(println(_))

  // headOption 
  println(
    OurLazyList("salmon", "shark", "tuna", "moray", "goldfish", "eel").
      filter(_ == "tuna").
      headOption)

      




