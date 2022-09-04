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



