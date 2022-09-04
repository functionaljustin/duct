// Reimplement OurLazyList from scratch and better
// This is a simplified version of Stream from Scala library 2.7
package examples

object LazyList2 extends App:

  trait OurLazyList[+A] {
    def head: A
    def tail: OurLazyList[A]

    def isEmpty: Boolean

    def forEach(f: A => Unit): Unit = {
      if !isEmpty then
        f(head)
        tail.forEach(f)
    }

    def take(n: Int): OurLazyList[A] = {
      if n == 0 || isEmpty then
        OurLazyList.empty
      else
        OurLazyList.cons(head, tail.take(n - 1))
    }

    def zip[B](other: OurLazyList[B]): OurLazyList[(A,B)] = {
     if isEmpty || other.isEmpty then
      OurLazyList.empty
    else
      OurLazyList.cons((head,other.head), tail.zip(other.tail))
    }

    def map[B](f: A => B): OurLazyList[B] = 
      if isEmpty then
        OurLazyList.empty
      else
        OurLazyList.cons(f(head), tail.map(f))
  }

  object OurLazyList:
    val empty = new OurLazyList[Nothing] {
      def head = throw new NoSuchElementException("No head of empty")
      def tail = throw new UnsupportedOperationException("No tail of empty")
      def isEmpty = true
    }

    def cons[A](hd: A, tl: => OurLazyList[A]) = new OurLazyList[A] {
      def head = hd

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

    def repeat[A](a: A): OurLazyList[A] = a #:: repeat(a)

  class Deferrer[A](tl: => OurLazyList[A]) {
    def #::(hd: A): OurLazyList[A] =
      OurLazyList.cons(hd, tl)
  }

  implicit def toDeferrer[A](l: => OurLazyList[A]): Deferrer[A] =
    new Deferrer[A](l)

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
  ones.take(5).forEach{ a =>
    println(a)
  }

  // zip
  val twos = OurLazyList.repeat(2)
  ones.zip(twos).take(5).forEach{ a =>
    println(a)
  }

  // map
  val threes = OurLazyList.repeat(3)
  val mapped = threes.take(3).map(_ + 1).forEach {
    a => println(a)
  }

  // fibs
  val fibs2: OurLazyList[BigInt] =
    BigInt(0) #:: BigInt(1) #::
      fibs2.zip(fibs2.tail).map { (a, b) =>
        println(s"Adding $a and $b")
        a + b
      }
  fibs2.take(10).forEach{
    a => println(a)
  }

  fibs2.take(11).forEach{
    a => println(a)
  }





