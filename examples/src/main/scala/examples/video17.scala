package examples

import scala.annotation.tailrec

// Code developed in the video https://youtu.be/laB15gG5bjY Ep 17: The Magic of LazyLists

object Video17 extends App:
    
    sealed trait OurLazyList[+A] {
        def head: A
        def tail: OurLazyList[A]
        def isEmpty: Boolean

        def headOption = if !isEmpty then     
                Some(head) 
            else 
                None
         def tailOption = if isEmpty then None
              else 
                if tail.isEmpty then None
                else Some(tail)

        def map[B](f: A => B): OurLazyList[B] = {
            if isEmpty then OurLazyList.empty
            else OurLazyList.cons(f(head), tail.map(f))
        }

        def zip[B](other: OurLazyList[B]): OurLazyList[(A,B)] = {
            if isEmpty || other.isEmpty then OurLazyList.empty
            else OurLazyList.cons((head, other.head), tail.zip(other.tail))
        }

        @tailrec
        final def dropWhile(f: A => Boolean): OurLazyList[A] = {
            if isEmpty then OurLazyList.empty
            else if f(head) then tail.dropWhile(f)
            else this
        }

        @tailrec
        final def last: A = {
            if isEmpty then throw new NoSuchElementException("Empty list has no last element")
            else {
                if this.tailOption.isEmpty then 
                    head
                else
                    tail.last
            }
        }    

        def filter(f: A => Boolean): OurLazyList[A] = {
            val dropped = this.dropWhile(a => !f(a))
            if dropped.isEmpty then OurLazyList.empty
            else OurLazyList.cons(dropped.head, dropped.tail.filter(f))    
        }    

        // Does not preserve laziness, may not terminate 
        // for infinite lists
        @tailrec    
        final def forall(f: A => Boolean): Boolean = {
            if isEmpty then true
            else f(head) && tail.forall(f)
        }

        def take(n: Int): OurLazyList[A] = {
            if n == 0 || isEmpty then OurLazyList.empty
            else OurLazyList.cons(head, tail.take(n -1))
        } 

        @tailrec
        final def drop(n: Int): OurLazyList[A] = {
            if n == 0 || isEmpty then this
            else tail.drop(n-1)
        } 

        def first(f: A => Boolean) = this.filter(f).headOption

        def exists(f: A => Boolean): Boolean = {
            this.first(f).isDefined
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

        def from(n: Int): OurLazyList[Int] = {
            n #:: from(n + 1)
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
