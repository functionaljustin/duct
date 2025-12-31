package examples

import java.util.concurrent.*
import java.util.concurrent.atomic.AtomicReference

object ParExample:
  
  // A Future is "registering a callback to be invoked when a result is ready"
  opaque type Future[+A] = (A => Unit) => Unit
  // Par requires applying an executor service which gives the Future
  opaque type Par[+A] = ExecutorService => Future[A]

  object Par:
    def unit[A](a: A): Par[A] =
      val f : Future[A] = cb => cb(a)
      val p : Par[A] = es => f
      p

    def delay[A](a: => A): Par[A] =
      es => cb => cb(a)

    def fork[A](a: => Par[A]): Par[A] =
      es => cb => eval(es)(a(es)(cb))

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def async[A](f: (A => Unit) => Unit): Par[A] = 
      es => cb => f(cb)

    extension [A](pa: Par[A])
      def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
        es => cb =>
          val futureA = CompletableFuture[A]()
          val futureB = CompletableFuture[B]()

          pa(es) { a => futureA.complete(a) }
          pb(es) { b => futureB.complete(b) }

          // Combine the results of both futures
          futureA.thenCombine(futureB, (a: A, b: B) => f(a, b))
                .whenComplete((result, ex) => 
                  if ex == null then cb(result)
                  else throw ex // Propagate any exceptions
                )

      def map2Actor[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
        es => cb =>
          var ar: Option[A] = None
          var br: Option[B] = None
          // Author note:
          // this implementation is a little too liberal in forking of threads -
          // it forks a new logical thread for the actor and for stack-safety,
          // forks evaluation of the callback `cb`
          // Justin note:
          // Combiner is an actor. the handler function that follows simply coordinates
          // the two Par executions of p and p2.
          // The first result to arrive is stored as an Option 
          // then when the second result arrives it is able to 
          // complete the callback.
          val combiner = Actor[Either[A,B]](es):
            case Left(a) =>
              if br.isDefined then eval(es)(cb(f(a, br.get)))
              else ar = Some(a)
            case Right(b) =>
              if ar.isDefined then eval(es)(cb(f(ar.get, b)))
              else br = Some(b)
          // Each par is evaluated and the callback sends the result
          // to the combiner actor.
          pa(es)(a => combiner ! Left(a))
          pb(es)(b => combiner ! Right(b))

      def runFromBook(es: ExecutorService): A =
        val ref = new AtomicReference[A]
        val latch = new CountDownLatch(1)
        pa(es) { a => ref.set(a); latch.countDown }
        latch.await
        ref.get

      def run(es: ExecutorService): A =
        val future = CompletableFuture[A]()
        pa(es) { a => future.complete(a) } // Set the result in the CompletableFuture
        future.join() // Block until the result is available and return it

  def main(args: Array[String]): Unit =
    import Par.*

    def example1(w: Int) = {
      Thread.sleep(w)
      println(s"example1 finished waiting for ${w}ms on thread ${Thread.currentThread.getName()}")
      w
    }

    val es = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)

    val par1 = Par.fork(Par.unit(example1(1230)))
    val par2 = Par.fork(Par.unit(example1(3770)))
    val result = par1.map2Actor(par2)((a,b) => a + b).run(es)
    println(s"Got $result from thread ${Thread.currentThread.getName()}")

    val result2 = par1.map2(par2)((a,b) => a + b).runFromBook(es)
    println(s"Got $result2 from thread ${Thread.currentThread.getName()}")
    es.shutdown()
