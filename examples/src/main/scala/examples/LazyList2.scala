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
  
  def tailWithSideEffect: OurLazyList[Int] = {
    println("getting empty OurLazyList")
    OurLazyList.empty
  }

  println("list1")
  val list1 = OurLazyList.cons(1, tailWithSideEffect) // SHOULD not print anything, and also is ok

  list1.tail // this would eval the tail

  println("forEach list1")
  list1.forEach{ a =>
    println(a)
  }






