package examples

object LazyListDemo extends App:
  // example of lazy evaluation
  class LazyThing[A](a: => A) {
    lazy val get = a
  }

  def f1 = {
    println("eval 10")
    10
  }

  val lazyThing1 = LazyThing(f1)

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
  class OurLazyList[A](
      head: Option[() => A],
      tail: Option[() => OurLazyList[A]]
  ) {
    lazy val getHead: Option[A] = head.map(f => f())
    lazy val getTail: Option[OurLazyList[A]] = tail.map(f => f())

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

    // def take(n: Int): OurLazyList[A] = {
    //   head match {
    //     case Some(hd) =>
    //       if (n == 0) {
    //         OurLazyList(None, None)
    //       } else {
    //         OurLazyList(Some(() => a), getTail.map(ta => () => ta.filter(f)))
    //       }
    //     case None =>
    //       OurLazyList(None, None)
    //   }
    // }

    def dropWhile(f: A => Boolean): OurLazyList[A] = {
      head match {
        case Some(hd) =>
          val a = getHead.get
          if (f(a)) {
            getTail
              .map(ta => ta.dropWhile(f))
              .getOrElse(OurLazyList(None, None))
          } else {
            OurLazyList(Some(() => a), getTail.map(ta => () => ta.dropWhile(f)))
          }
        case None =>
          OurLazyList(None, None)
      }
    }

    def filter(f: A => Boolean): OurLazyList[A] = {
      this.dropWhile(a => !f(a))
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

  def evalNum(n: Int) = {
    println(s"evaluating $n")
    n
  }

  println("list things")

  val l1 = (1 to 4).map(a => () => evalNum(a))
  val lazyL1 = OurLazyList.fromSeq(l1)
  val lazyL2 = lazyL1.map(_ + 1)
  val lazyL3 = lazyL2.filter(_ % 2 == 0)
  // val lazyL4 = lazyL2.map(_ + 5)
  // val dropped = lazyL1.dropWhile(_ < 5)

  println("foreach")

  // println("l1")
  // lazyL1.forEach(a => println(a))
  // println("l2")
  // lazyL2.forEach(a => println(a))
  println("l3")
  lazyL3.forEach(a => println(a))
  // println("l4")
  // lazyL4.forEach(a => println(a))

  // println("dropped")
  // dropped.forEach(a => println(a))

  val lazyConsedList1 =
    evalNum(1) ##:: evalNum(2) ##:: evalNum(3) ##:: OurLazyList(None, None)
  lazyConsedList1.forEach(a => println(s"crazy!$a"))

  // TODO enough for fib seq need tail and zip
