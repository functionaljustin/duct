package examples

import scala.annotation.tailrec

object LazyList2 extends App:

  trait OurLazyList[+A] {
    def head: A
    def tail: OurLazyList[A]

    def isEmpty: Boolean

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

    @tailrec
    final def foldLeft[B](z: B)(f: (B, A) => B): B = {
      if isEmpty then z
      else tail.foldLeft(f(z, head))(f)
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

      private var tailEvaluated = false
      private var tailValue: OurLazyList[A] = _

      def tail: OurLazyList[A] = {
        if (!tailEvaluated) {
          tailValue = tl
          tailEvaluated = true
        }
        tailValue
      }

      def isEmpty = false
    }

    def apply[A](as: A*): OurLazyList[A] = {
      if as.isEmpty then OurLazyList.empty
      else cons(as.head, apply(as.tail: _*))
    }

    def repeat[A](a: A): OurLazyList[A] = a #:: repeat(a)

  class Deferrer[A](tl: => OurLazyList[A]) {
    def #::(hd: A): OurLazyList[A] =
      OurLazyList.cons(hd, tl)
  }

  implicit def toDeferrer[A](l: => OurLazyList[A]): Deferrer[A] =
    new Deferrer[A](l)

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

  // fibs
  val fibs2: OurLazyList[BigInt] =
    BigInt(0) #:: BigInt(1) #::
      fibs2.zip(fibs2.tail).map { (a, b) =>
        println(s"Adding $a and $b")
        a + b
      }
  fibs2.take(10).forEach { a =>
    println(a)
  }

  fibs2.take(11).forEach { a =>
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

  println(incN(1,1).take(10000000).foldLeft(BigInt(0)){case (acc,a) => acc + a})

  // // forEach works with large data sets too but is much slower (about 2 minutes)
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

