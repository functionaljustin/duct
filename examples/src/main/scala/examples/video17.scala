package examples

object video17 extends App:
    
    sealed trait OurLazyList[+A] {
        def head: A
        def tail: OurLazyList[A]
        def isEmpty: Boolean

        def map[B](f: A => B): OurLazyList[B] = {
            if isEmpty then OurLazyList.empty
            else OurLazyList.cons(f(head), tail.map(f))
        }

        def dropWhile(f: A => Boolean): OurLazyList[A] = {
            if isEmpty then OurLazyList.empty
            else if f(head) then tail.dropWhile(f)
            else this
        }    
    }

    object OurLazyList:
        val empty = new OurLazyList[Nothing] {
            def head = throw new NoSuchElementException("Cannot get head of empty lazy list")
            def tail = throw new UnsupportedOperationException("No tail of empty lazy list")
            val isEmpty = true
        }

        def cons[A](hd: => A, tl: => OurLazyList[A]) = new OurLazyList[A] {
            lazy val head = hd
            lazy val tail = tl
            val isEmpty = false
        }    

        def apply[A](as: A*): OurLazyList[A] = {
            if as.isEmpty then OurLazyList.empty
            else OurLazyList.cons(as.head, apply(as.tail: _*))
        }    

    val nums = OurLazyList(1,2,3,4,5)
    val dropped = nums.dropWhile(_ < 3)
    println(dropped.head)





