package examples

import scala.language.postfixOps
import scala.math.BigInt
import org.functionaljustin.duct.datatypes.{LazyList,#::}
import scala.language.implicitConversions

object LazyListDemo extends App:
  // example of lazy evaluation
  class LazyThing[A](a: => A) {
    lazy val get = a
  }

  val lazyThing1 = LazyThing({ println("side effect"); 11 })

  println("Print lazy thing twice...")
  println(lazyThing1.get)
  println(lazyThing1.get)

  object LazyList:
    def empty[A] = LazyList[A](None,None)

    def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
      LazyList(Some(() => hd), Some(() => tl))

    def fromSeq[A](in: Seq[() => A]): LazyList[A] = {
      in match {
        case first +: rest =>
          LazyList(Some(first), Some(() => fromSeq(rest)))
        case _ => LazyList(None, None)
      }
    }
  // Implementing a lazy list
  case class LazyList[A](
      head: Option[() => A],
      tail: Option[() => LazyList[A]]
  ) {
    def isEmpty = head.isEmpty

    lazy val getHead: Option[A] = head.map(f => f())
    lazy val getTail: Option[LazyList[A]] = tail.map(f => f())

    // safeTail should return an LazyList without evaluating with the head or the tail
    def safeTail: LazyList[A] = {
      if head.isEmpty || tail.isEmpty then LazyList(None, None)
      else {
        val tl = tail.get
        LazyList(Some(() => tl().getHead.get), Some(() => tl().safeTail))
      }
    }

    def map[B](f: A => B): LazyList[B] = {
      head match {
        case Some(hd) =>
          LazyList(
            {
              Some(() => {
                val a = getHead.get
                println(s"map applying f to $a")
                f(getHead.get)
              })
            },
            getTail.map(ta => () => ta.map(f))
          )
        case None =>
          LazyList(None, None)
      }
    }

    // Returns a new lazy list that terminates after n elements
    def take(n: Int): LazyList[A] = {
      println(s"take $n")
      head match {
        case Some(hd) =>
          if (n == 0) {
            LazyList(None, None)
          } else {
            LazyList(Some(hd), getTail.map(ta => () => ta.take(n - 1)))
          }
        case None =>
          LazyList(None, None)
      }
    }

    def dropWhile(f: A => Boolean): LazyList[A] = {
      println("dropWhile")
      head match {
        case Some(hd) =>
          val a = getHead.get
          if (f(a)) {
            getTail
              .map(ta => ta.dropWhile(f))
              .getOrElse(LazyList(None, None))
          } else {
            LazyList(Some(() => a), getTail.map(ta => () => ta))
          }
        case None =>
          LazyList(None, None)
      }
    }

    def filter(f: A => Boolean): LazyList[A] = {
      println("filter")
      val rest = this.dropWhile(a => !f(a))
      if (rest.isEmpty)
        rest
      else
        LazyList(
          rest.getHead.map(a => () => a),
          rest.getTail.map(ta => () => ta.filter(f))
        )
    }

    // Materialize as a list
    def toList(): List[A] = {
      val lb = List.newBuilder[A]
      this.forEach(a => lb.addOne(a))
      lb.result()
    }

    // Zip two lazylists together until one of them runs out of things
    def zip[B](other: => LazyList[B]): LazyList[(A, B)] = {
      if this.isEmpty || other.isEmpty then LazyList(None, None)
      else
        LazyList(
          Some(() => (this.getHead.get, other.getHead.get)),
          this.getTail.map(ta => () => ta.zip(other.getTail.get))
        )
    }

    def forEach(f: A => Unit): Unit = {
      def forEachHelper(f: A => Unit, in: LazyList[A]): Unit = {
        in.getHead.foreach(a =>
          println(s"foreach sees $a")
          f(a)
          in.getTail.foreach(tail => forEachHelper(f, tail))
        )
      }
      forEachHelper(f, this)
    }

  }

  // based on scala lib
  class Deferrer[A] (l: => LazyList[A]) {
    def ##::(newHead: => A): LazyList[A] = 
      LazyList.cons(newHead,l)
  }

  implicit def toDeferrer[A](l: => LazyList[A]): Deferrer[A] = new Deferrer[A](l)

  val l1 = (1 to 20).map(a => () => { println(s"evaluating $a"); a })

  val lazyL1 = LazyList.fromSeq(l1)
  assert(
    lazyL1.filter(n => n % 2 == 0).take(5).toList() == List(2, 4, 6, 8, 10)
  )

  val l2 = (1 to 5).map(a => () => { println(s"evaluating $a"); a })
  val l3 = (1 to 10).map(a => () => { println(s"evaluating $a"); a })
  val zipped23 = LazyList.fromSeq(l2).zip(LazyList.fromSeq(l2))
  assert(zipped23.toList() == List((1, 1), (2, 2), (3, 3), (4, 4), (5, 5)))

  def repeatN(n: Int): LazyList[Int] =
    LazyList(Some(() => n), Some(() => repeatN(n)))
  assert(repeatN(5).take(5).toList() == List(5, 5, 5, 5, 5))

  def incN(n: Int, inc: Int): LazyList[Int] =
    LazyList(Some(() => n), Some(() => incN(n + inc, inc)))
  assert(incN(5, 2).take(5).toList() == List(5, 7, 9, 11, 13))

  assert(
    repeatN(3).zip(incN(5, 5)).map(a => a._1 + a._2).take(5).toList() == List(8,
      13, 18, 23, 28)
  )

  // This is an example from Scala lib
  // val fibs2: LazyList[BigInt] =
  //   BigInt(0) #:: BigInt(1) #::
  //     fibs2.zip(fibs2.tail).map { (a, b) =>
  //       println(s"Adding $a and $b")
  //       a + b
  //     }
  // fibs2.take(10).foreach(println)

  lazy val fib: LazyList[Int] = {
    def loop(h: Int, n: Int): LazyList[Int] = {
      LazyList.cons(h, loop(n, h + n))
    }
    loop(1, 1)
  }
  assert(fib.take(10).toList() == List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55))

  def tailWithSideEffect: LazyList[Int] = {
    println("getting empty LazyList")
    LazyList(None,None)
  }

  // From Scala lib doc
  // The head, the tail and whether the list is empty or not can be initially unknown.
  // Once any of those are evaluated, they are all known, though if the tail is
  // built with `#::` or `#:::`, it's content still isn't evaluated. Instead, evaluating
  // the tails content is deferred until the tails empty status, head or tail is
  // evaluated.

  println("eval tail")
  val emptyTail = tailWithSideEffect 
  println("suspended")
  val suspended = 1 ##:: tailWithSideEffect // SHOULD not print anything and doesn't
  println("suspended3")
  val suspended3 = LazyList.cons(1, tailWithSideEffect) // SHOULD not print anything, and also is ok
  // val tail = suspended.tail // Is not evaluation but should be of type LazyList
  println("suspendedCons")
  val suspendedCons = LazyList.cons(1,tailWithSideEffect) // Suspends correctly
  suspendedCons.isEmpty // Should unsuspend but does not

  // TODO make cons work with ##::
  // It doesn't work due to a lazy eval violation with fibs.tail
  // val fibs: LazyList[BigInt] = {
  //   cons(
  //     BigInt(0),
  //     cons(
  //       BigInt(1),
  //       fibs.zip(fibs.safeTail).map { n =>
  //         n._1 + n._2
  //       }
  //     )
  //   )
  // }

  // fibs.take(10).forEach(a => println(s"lol $a"))


  def tailWithSideEffect2: LazyList[Int] = {
    println("getting empty LazyList")
    LazyList.empty
  }

  // From Scala lib doc
  // The head, the tail and whether the list is empty or not can be initially unknown.
  // Once any of those are evaluated, they are all known, though if the tail is
  // built with `#::` or `#:::`, it's content still isn't evaluated. Instead, evaluating
  // the tails content is deferred until the tails empty status, head or tail is
  // evaluated.

  println("check if empty")
  val emptyTail2 = tailWithSideEffect2
  println("suspend with #::")
  val suspended2 = 1 ##:: tailWithSideEffect2 // SHOULD not print anything, but it does in my code
  println("eval tail")
  val tail2 = suspended2.tail // Is not evaluation but should be of type LazyList
  println("not evalled")
  println(tail2.isEmpty) // this evals
  // val suspendedCons2 = cons(1,tailWithSideEffect) // Suspends correctly
  println("check if empty")
  suspended2.isEmpty // Should unsuspend but does not

  // Zip two lazy lists with a map function that combines them to some new type
  def zipWith[A, B, C](as: LazyList[A], bs: LazyList[B])(
      f: (A, B) => C): LazyList[C] = {
    as.zip(bs).map { case (a, b) => f(a, b) }
  }

  def repeat[A](a: A): LazyList[A] = a ##:: repeat(a)

  // test zipWith and repeat (this is okay)
  // val ones = repeat(1)
  // val threeFactors = incN(3,3)
  // val zw1 = zipWith(ones,threeFactors){case (a,b) => a * b}
  // zw1.take(10).forEach {
  //   println(_)
  // }


  // TODO tranpose needs an implementation of unapply for the pattern match
  // def transpose[A](matrix: LazyList[LazyList[A]]): LazyList[LazyList[A]] = {
  //   matrix match {
  //     case LazyList(_,_) => repeat(LazyList.empty[A])
  //     case xs ##:: xss =>
  //       zipWith(xs, transpose(xss)) {
  //         case (a, as) =>
  //           a +: as
  //       }
  //   }
  // }

  // val matrix = LazyList(
  //     LazyList(1, 2, 3, 4, 5),
  //     LazyList(6, 7, 8, 9, 10),
  //     LazyList(11, 12, 13, 14, 15)
  //   )

  // matrix.foreach { l =>
  //     l.foreach { l2 =>
  //       print(f"$l2 ")
  //     }
  //     println()
  //   }

  // val transposed = transpose(matrix)

  // transposed.foreach { l =>
  //     l.foreach { l2 =>
  //       print(f"$l2 ")
  //     }
  //     println()
  //   } 
