package examples

import scala.language.postfixOps
import scala.math.BigInt

object LazyListDemo extends App:
  // example of lazy evaluation
  class LazyThing[A](a: => A) {
    lazy val get = a
  }

  val lazyThing1 = LazyThing({ println("side effect"); 11 })

  println("Print lazy thing twice...")
  println(lazyThing1.get)
  println(lazyThing1.get)

  object OurLazyList:
    def fromSeq[A](in: Seq[() => A]): OurLazyList[A] = {
      in match {
        case first +: rest =>
          OurLazyList(Some(first), Some(() => fromSeq(rest)))
        case _ => OurLazyList(None, None)
      }
    }
    def empty[A] = OurLazyList[A](None,None)

  // Implementing a lazy list
  case class OurLazyList[A](
      head: Option[() => A],
      tail: Option[() => OurLazyList[A]]
  ) {
    def isEmpty = head.isEmpty

    lazy val getHead: Option[A] = head.map(f => f())
    lazy val getTail: Option[OurLazyList[A]] = tail.map(f => f())

    // safeTail should return an OurLazyList without evaluating with the head or the tail
    def safeTail: OurLazyList[A] = {
      if head.isEmpty || tail.isEmpty then OurLazyList(None, None)
      else {
        val tl = tail.get
        OurLazyList(Some(() => tl().getHead.get), Some(() => tl().safeTail))
      }
    }

    def map[B](f: A => B): OurLazyList[B] = {
      head match {
        case Some(hd) =>
          OurLazyList(
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
          OurLazyList(None, None)
      }
    }

    // Returns a new lazy list that terminates after n elements
    def take(n: Int): OurLazyList[A] = {
      println(s"take $n")
      head match {
        case Some(hd) =>
          if (n == 0) {
            OurLazyList(None, None)
          } else {
            OurLazyList(Some(hd), getTail.map(ta => () => ta.take(n - 1)))
          }
        case None =>
          OurLazyList(None, None)
      }
    }

    def dropWhile(f: A => Boolean): OurLazyList[A] = {
      println("dropWhile")
      head match {
        case Some(hd) =>
          val a = getHead.get
          if (f(a)) {
            getTail
              .map(ta => ta.dropWhile(f))
              .getOrElse(OurLazyList(None, None))
          } else {
            OurLazyList(Some(() => a), getTail.map(ta => () => ta))
          }
        case None =>
          OurLazyList(None, None)
      }
    }

    def filter(f: A => Boolean): OurLazyList[A] = {
      println("filter")
      val rest = this.dropWhile(a => !f(a))
      if (rest.isEmpty)
        rest
      else
        OurLazyList(
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
    def zip[B](other: => OurLazyList[B]): OurLazyList[(A, B)] = {
      if this.isEmpty || other.isEmpty then OurLazyList(None, None)
      else
        OurLazyList(
          Some(() => (this.getHead.get, other.getHead.get)),
          this.getTail.map(ta => () => ta.zip(other.getTail.get))
        )
    }

    def forEach(f: A => Unit): Unit = {
      def forEachHelper(f: A => Unit, in: OurLazyList[A]): Unit = {
        in.getHead.foreach(a =>
          println(s"foreach sees $a")
          f(a)
          in.getTail.foreach(tail => forEachHelper(f, tail))
        )
      }
      forEachHelper(f, this)
    }

  }

  def cons[A](hd: => A, tl: => OurLazyList[A]): OurLazyList[A] = {
    OurLazyList(Some(() => hd), Some(() => tl))
  }

  // based on scala lib
  class Deferrer[A] (private val l: OurLazyList[A]) extends AnyVal {
    def ##::(newHead: => A): OurLazyList[A] = 
      cons(newHead,l)
  }

  implicit def toDeferrer[A](l: => OurLazyList[A]): Deferrer[A] = new Deferrer[A](l)

  val l1 = (1 to 20).map(a => () => { println(s"evaluating $a"); a })

  val lazyL1 = OurLazyList.fromSeq(l1)
  assert(
    lazyL1.filter(n => n % 2 == 0).take(5).toList() == List(2, 4, 6, 8, 10)
  )

  val l2 = (1 to 5).map(a => () => { println(s"evaluating $a"); a })
  val l3 = (1 to 10).map(a => () => { println(s"evaluating $a"); a })
  val zipped23 = OurLazyList.fromSeq(l2).zip(OurLazyList.fromSeq(l2))
  assert(zipped23.toList() == List((1, 1), (2, 2), (3, 3), (4, 4), (5, 5)))

  def repeatN(n: Int): OurLazyList[Int] =
    OurLazyList(Some(() => n), Some(() => repeatN(n)))
  assert(repeatN(5).take(5).toList() == List(5, 5, 5, 5, 5))

  def incN(n: Int, inc: Int): OurLazyList[Int] =
    OurLazyList(Some(() => n), Some(() => incN(n + inc, inc)))
  assert(incN(5, 2).take(5).toList() == List(5, 7, 9, 11, 13))

  assert(
    repeatN(3).zip(incN(5, 5)).map(a => a._1 + a._2).take(5).toList() == List(8,
      13, 18, 23, 28)
  )

  // This is an example from Scala lib
  val fibs2: LazyList[BigInt] =
    BigInt(0) #:: BigInt(1) #::
      fibs2.zip(fibs2.tail).map { (a, b) =>
        println(s"Adding $a and $b")
        a + b
      }
  fibs2.take(10).foreach(println)

  lazy val fib: OurLazyList[Int] = {
    def loop(h: Int, n: Int): OurLazyList[Int] = {
      cons(h, loop(n, h + n))
    }
    loop(1, 1)
  }
  assert(fib.take(10).toList() == List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55))

  def tailWithSideEffect: OurLazyList[Int] = {
    println("getting empty OurLazyList")
    OurLazyList(None,None)
  }

  // From Scala lib doc
  // The head, the tail and whether the list is empty or not can be initially unknown.
  // Once any of those are evaluated, they are all known, though if the tail is
  // built with `#::` or `#:::`, it's content still isn't evaluated. Instead, evaluating
  // the tails content is deferred until the tails empty status, head or tail is
  // evaluated.

  val emptyTail = tailWithSideEffect 
  val suspended = 1 ##:: tailWithSideEffect // SHOULD not print anything, but it does
  val tail = suspended.tail // Is not evaluation but should be of type OurLazyList
  val suspendedCons = cons(1,tailWithSideEffect) // Suspends correctly
  suspendedCons.isEmpty // Should unsuspend but does not

  // TODO make cons work with ##::
  // It doesn't work due to a lazy eval violation with fibs.tail
  // val fibs: OurLazyList[BigInt] = {
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

  val emptyTail2 = tailWithSideEffect2
  val suspended2 = 1 #:: tailWithSideEffect2 // SHOULD not print anything, but it does in my code
  val tail2 = suspended2.tail // Is not evaluation but should be of type OurLazyList
  // val suspendedCons2 = cons(1,tailWithSideEffect) // Suspends correctly
  suspended2.isEmpty // Should unsuspend but does not

  // Zip two lazy lists with a map function that combines them to some new type
  def zipWith[A, B, C](as: OurLazyList[A], bs: OurLazyList[B])(
      f: (A, B) => C): OurLazyList[C] = {
    as.zip(bs).map { case (a, b) => f(a, b) }
  }

  // TODO repeat works but only with cons, not with ##::
  // def repeat[A](a: A): OurLazyList[A] = a ##:: repeat(a)
  def repeat[A](a: A): OurLazyList[A] = cons(a, repeat(a))

  // TODO test zipWith and repeat
  val ones = repeat(1)
  val threeFactors = incN(3,3)
  val zw1 = zipWith(ones,threeFactors){case (a,b) => a * b}
  zw1.take(10).forEach {
    println(_)
  }


  // TODO tranpose needs an implementation of unapply for the pattern match
  // def transpose[A](matrix: OurLazyList[OurLazyList[A]]): OurLazyList[OurLazyList[A]] = {
  //   matrix match {
  //     case OurLazyList(_,_) => repeat(OurLazyList.empty[A])
  //     case xs ##:: xss =>
  //       zipWith(xs, transpose(xss)) {
  //         case (a, as) =>
  //           a +: as
  //       }
  //   }
  // }

  // val matrix = OurLazyList(
  //     OurLazyList(1, 2, 3, 4, 5),
  //     OurLazyList(6, 7, 8, 9, 10),
  //     OurLazyList(11, 12, 13, 14, 15)
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
