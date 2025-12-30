package examples

import java.util.concurrent.*
import java.util.concurrent.atomic.AtomicReference

object ParExample extends App:
  
  opaque type Future[+A] = (A => Unit) => Unit
  opaque type Par[+A] = ExecutorService => Future[A]

  object Par:
    def unit[A](a: A): Par[A] =
      es => cb => cb(a)

    def delay[A](a: => A): Par[A] =
      es => cb => cb(a)

    def fork[A](a: => Par[A]): Par[A] =
      es => cb => eval(es)(a(es)(cb))

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def async[A](f: (A => Unit) => Unit): Par[A] = 
      es => cb => f(cb)

    def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => cb =>
        val futureA = CompletableFuture[A]()
        val futureB = CompletableFuture[B]()

        p1(es) { a => futureA.complete(a) }
        p2(es) { b => futureB.complete(b) }

        // Combine the results of both futures
        futureA.thenCombine(futureB, (a: A, b: B) => f(a, b))
               .whenComplete((result, ex) => 
                 if ex == null then cb(result)
                 else throw ex // Propagate any exceptions
               )

  extension [A](pa: Par[A]) def run(es: ExecutorService): A =
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    pa(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get

  val es = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)


  def p1(w: Int) = {
    Thread.sleep(w)
    println(s"p1 finished waiting for ${w}ms on thread ${Thread.currentThread.getName()}")
    w
  }

  val par1 = Par.fork(Par.delay(p1(123)))
  val par2 = Par.fork(Par.delay(p1(377)))
  val result = Par.map2(par1, par2)((a,b) => a + b).run(es)
  println(s"Got $result from thread ${Thread.currentThread.getName()}")

  es.shutdown()
