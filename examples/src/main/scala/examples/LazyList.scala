package examples

object LazyListDemo extends App:
  // example of lazy evaluation
  class LazyThing[A](a: => A) {
    lazy val get = a
  }

  val f1 = {
    println("eval 10")
    10
  }

  val lazyThing1 = LazyThing(f1)

  println(lazyThing1.get)
  println(lazyThing1.get)

  object OurLazyList:
    def fromSeq[A](in: Seq[A]): OurLazyList[A] = {
      in match {
        case first :: rest => 
          OurLazyList(Some(() => first), Some(() => fromSeq(rest)))
        case Nil => OurLazyList(None,None)
      }
    }

  // Implementing a lazy list
  class OurLazyList[A](head: Option[() => A], tail: Option[() => OurLazyList[A]]) {
    lazy val getHead: Option[A] = head.map(f => f())
    lazy val getTail: Option[OurLazyList[A]] = tail.map(f => f())

    def forEach(f: A => Unit): Unit = {
      def forEachHelper(f: A => Unit, in: OurLazyList[A]): Unit = {
        in.getHead match {
          case Some(a) => {
            f(a)
            in.getTail match {
              case Some(tail) => 
                forEachHelper(f,tail)
              case None =>
                None
            }
          }
          case None => ()
        }
      }
      forEachHelper(f, this)
    }
  }

  def evalNum(n: Int) = {
    println(s"evaluating $n")
    n
  }

  val l1 = List(1,2,3).map(evalNum)
  val lazyL1 = OurLazyList.fromSeq(l1)
  lazyL1.forEach(a => 
      println(a))
  lazyL1.forEach(a => 
      println(a))