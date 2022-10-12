package examples

import scala.concurrent.Future
import scala.io.StdIn.readLine

// IO Monad from the Red Book

object IOMonad extends App:

  enum IO[A]:
    case Return(a: A)
    case Suspend(resume: () => A)
    case FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)

    def map[B](f: A => B): IO[B] =
      flatMap(a => Return(f(a)))

    @annotation.tailrec final def unsafeRun(): A = this match
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match
        case Return(a) => f(a).unsafeRun()
        case Suspend(r) => f(r()).unsafeRun()
        case FlatMap(y, g) => y.flatMap(a => g(a).flatMap(f)).unsafeRun()

  object IO:
    def apply[A](a: => A): IO[A] = suspend(Return(a))
    def suspend[A](a: => IO[A]): IO[A] =
      Suspend(() => a).flatMap(identity)

  def printIO(s: String): IO[Unit] = {
    IO.Suspend(() => print(s))
  }

  def fahrenheitToCelsius(f: Double): Double = (f - 32.0) * (5.0/9.0)

  def ReadLine: IO[String] = IO.apply(readLine())
  def PrintLine(msg: String): IO[Unit] = IO(println(msg))

  def converter: IO[Unit] = for
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  yield ()  

  converter.unsafeRun()

 // printIO("poop\n").flatMap(_ => printIO("haha")).unsafeRun()
