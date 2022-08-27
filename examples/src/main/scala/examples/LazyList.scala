package examples

import scala.language.postfixOps

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
      if head.isEmpty || tail.isEmpty then
        OurLazyList(None,None)
      else {
        // val newHead = tail.map(ta => ta().head)
        OurLazyList(None,None)
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
    def zip[B](other: OurLazyList[B]): OurLazyList[(A, B)] = {
      if this.isEmpty || other.isEmpty then
        OurLazyList(None, None)
      else
        OurLazyList(Some(() => (this.getHead.get, other.getHead.get)),
          this.getTail.map(ta => () =>
            ta.zip(other.getTail.get)))
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

    // creation
    def ##::(newHead: => A): OurLazyList[A] = {
      OurLazyList(Some(() => newHead), Some(() => this))
    }

  }

  val l1 = (1 to 20).map(a => () => { println(s"evaluating $a"); a })

  val lazyL1 = OurLazyList.fromSeq(l1)
  assert(
    lazyL1.filter(n => n % 2 == 0).take(5).toList() == List(2, 4, 6, 8, 10)
  )

  val l2 = (1 to 5).map(a => () => { println(s"evaluating $a"); a })
  val l3 = (1 to 10).map(a => () => { println(s"evaluating $a"); a })
  val zipped23 = OurLazyList.fromSeq(l2).zip(OurLazyList.fromSeq(l2))
  assert(zipped23.toList() == List((1, 1), (2, 2), (3, 3), (4, 4), (5, 5)))

  // TODO enough for fib seq need tail and zip
  import scala.math.BigInt
  // OurLazyList(None,None)
  val fibs: OurLazyList[BigInt] =
    BigInt(0) ##:: BigInt(1) ##:: fibs.zip(fibs.safeTail).map { n =>
        n._1 + n._2
      }
  fibs.take(5).forEach(println)

   val fibs2: LazyList[BigInt] =
         BigInt(0) #:: BigInt(1) #::
           fibs2.zip(fibs2.tail).map{ (a,b) =>
             println(s"Adding $a and $b")
             a + b
           }
   fibs2.take(5).foreach(println)

