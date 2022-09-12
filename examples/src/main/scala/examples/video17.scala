package examples

import scala.annotation.tailrec

object video17 extends App:
    
    sealed trait OurLazyList[+A] {
        def head: A
        def tail: OurLazyList[A]
        def isEmpty: Boolean

        def map[B](f: A => B): OurLazyList[B] = {
            if isEmpty then OurLazyList.empty
            else OurLazyList.cons(f(head), tail.map(f))
        }

        def zip[B](other: OurLazyList[B]): OurLazyList[(A,B)] = {
            if isEmpty || other.isEmpty then OurLazyList.empty
            else OurLazyList.cons((head, other.head), tail.zip(other.tail))
        }

        def dropWhile(f: A => Boolean): OurLazyList[A] = {
            if isEmpty then OurLazyList.empty
            else if f(head) then tail.dropWhile(f)
            else this
        }

        def filter(f: A => Boolean): OurLazyList[A] = {
            val dropped = this.dropWhile(a => !f(a))
            if dropped.isEmpty then OurLazyList.empty
            else OurLazyList.cons(dropped.head, dropped.tail.filter(f))    
        }    

        def take(n: Int): OurLazyList[A] = {
            if n == 0 || isEmpty then OurLazyList.empty
            else OurLazyList.cons(head, tail.take(n -1))
        } 

        @tailrec
        final def forEach(f: A => Unit): Unit = {
            if !isEmpty then
                f(head)
                tail.forEach(f)
        }

        @tailrec
        final def foldLeft[B](z: B)(f: (B, A) => B): B = {
            if isEmpty then z
            else tail.foldLeft(f(z, head))(f)
        }

        def foldRight[B](z: => B)(f: (A, => B) => B): B = {
            if isEmpty then z
            else
                f(head, tail.foldRight(z)(f))
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

        extension [A](hd: => A)
            def #::(tl: => OurLazyList[A]): OurLazyList[A] = 
                OurLazyList.cons(hd, tl)

    object #:: {
        def unapply[A](s: OurLazyList[A]): Option[(A, OurLazyList[A])] =
            if !s.isEmpty then Some((s.head, s.tail)) else None
    }

    val fish = "salmon" #:: "shark" #:: "tuna" #:: "moray" #:: "goldfish" #:: "eel" #:: OurLazyList.empty
   
    println(fish.foldRight(List("barracuda")){
        (a, acc) => 
            println(a)
            if a == "tuna" then 
                List("tuna")
            else
                acc :+ a
    })

  





