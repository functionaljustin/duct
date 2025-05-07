// Conways game of life with Scala and Comonads, animated on the terminal
package examples

import org.functionaljustin.duct.typeclasses.comonad.Comonad
import org.functionaljustin.duct.typeclasses.applicative.{given,*}
import org.functionaljustin.duct.typeclasses.semigroup.{given,*}
import scala.collection.immutable.Vector

// Show typeclass for displaying values in a type safe way
trait Show[A]:
  extension(a: A)
    def show: String

given showInt: Show[Int] with
  extension (i: Int)
    def show: String = i.toString

given showChar: Show[Char] with
  extension (c: Char)
    def show: String = c.toString

// given showList[A: Show]: Show[List[A]] with
//   extension (xs: List[A]) def show: String =
//     xs match
//       case Nil => "Nil"
//       case x :: xs => s"${x.show} :: ${xs.show}"

object FocusedGrid {

  // FocusedGrid is a 2d array like Vector[Vector[A]] and a focus point that is a row
  // and column index into the grid
  case class FocusedGrid[A](focus: Tuple2[Int, Int], grid: Vector[Vector[A]])

  // Create a focused grid of the specified size, filling each point with the
  // specified value
  def filledFocusGrid[A](a: A, width: Int, height: Int) = {
    val row = Vector.fill(width)(a)
    FocusedGrid((0,0), Vector.fill(height)(row))
  }

  // Implementation of Show that gives us a type safe way to display
  // a FocusedGrid of any type A (which also has a Show instance)
  given focusedGridShow[A: Show]: Show[FocusedGrid[A]] with
    extension (fg: FocusedGrid[A])
      def show: String =
        fg.grid
          .map { row =>
            row.iterator.map(_.show).mkString("")
          }
          .mkString("\n")

  // Implement Comonad for FocusedGrid. Note that this gives us map as well since
  // Comonads are Functors
  // Note we're also extending Apply which requires us to implement ap
  given focusedGridComonad: Comonad[FocusedGrid] with

    def coflatten[A](fa: FocusedGrid[A]): FocusedGrid[FocusedGrid[A]] = {
      val grid = fa.grid.zipWithIndex.map((row, ri) => row.zipWithIndex.map((col, ci) => FocusedGrid((ri, ci), fa.grid)))
      FocusedGrid(fa.focus, grid)
    }

    extension [A,B](fa: FocusedGrid[A]) 
      def extract: A = fa.grid(fa.focus._1)(fa.focus._2)
      def coflatMap(f: FocusedGrid[A] => B): FocusedGrid[B] = 
        val grid = coflatten(fa).grid.map(_.map(col => f(col)))
        FocusedGrid(fa.focus, grid)
      def map(f: A => B): FocusedGrid[B] =
        FocusedGrid(fa.focus, fa.grid.map(row => row.map(a => f(a))))
}
  
//         def coflatMap(f: NonEmptyList[A] => B): NonEmptyList[B] = nel.tails.map(f)
//         def map(f: A => B): NonEmptyList[B] = nel.map(f)

//     override def coflatten[A](fa: FocusedGrid[A]): FocusedGrid[FocusedGrid[A]] = {
//       val grid = fa.grid.mapWithIndex((row, ri) => row.mapWithIndex((col, ci) => FocusedGrid((ri, ci), fa.grid)))
//       FocusedGrid(fa.focus, grid)
//     }

//     // Gives us all of the possible foci for this grid
//     def coflatMap[A, B](fa: FocusedGrid[A])(f: FocusedGrid[A] => B): FocusedGrid[B] = {
//       val grid = coflatten(fa).grid.map(_.map(col => f(col)))
//       FocusedGrid(fa.focus, grid)
//     }

//     // extract simply returns the A at the focus
//     def extract[A](fa: FocusedGrid[A]): A = fa.grid(fa.focus._1)(fa.focus._2)

//     def pure[A](a: A): FocusedGrid[A] = FocusedGrid((0,0), Vector(Vector(a)))

//     // This is an optimized map2 that avoids creating intermediate structures
//     // by using iterators
//     override def map2[A, B, Z](fa: FocusedGrid[A], fb: FocusedGrid[B])(f: (A, B) => Z): FocusedGrid[Z] = {
//       val faRowIter = fa.grid.iterator
//       val fbRowIter = fb.grid.iterator
//       val rowBuilder = Vector.newBuilder[Vector[Z]]

//       while(faRowIter.hasNext && fbRowIter.hasNext) {
//         val faColIter = faRowIter.next.iterator
//         val fbColIter = fbRowIter.next.iterator
//         val colBuilder = Vector.newBuilder[Z]

//         while(faColIter.hasNext && fbColIter.hasNext) {
//           colBuilder.addOne(f(faColIter.next, fbColIter.next))
//         }
//         rowBuilder.addOne(colBuilder.result)
//       }
//       FocusedGrid(fa.focus, rowBuilder.result)
//     }

//     // This is a nicer but less performant map2
//     def map2Slow[A, B, Z](fa: FocusedGrid[A], fb: FocusedGrid[B])(f: (A, B) => Z): FocusedGrid[Z] = {
//       val newGrid = fa.grid.zip(fb.grid).map {
//         case (rowA, rowB) =>
//           rowA.zip(rowB).map {
//             case (colA, colB) =>
//               f(colA,colB)
//           }
//       }
//       FocusedGrid(fa.focus, newGrid)
//     }

//     // Note that we have an efficient implementation of map2 for this type class
//     // instance, so I use that to implement ap
//     def ap[A, B](fab: FocusedGrid[A => B])(fa: FocusedGrid[A]): FocusedGrid[B] = {
//       map2(fab, fa){
//         (f,a) =>
//           f(a)
//       }
//     }

//     // You can write ap in terms of map alone ...
//     def apSlow[A, B](ff: FocusedGrid[A => B])(fa: FocusedGrid[A]): FocusedGrid[B] = {
//       val newGrid = ff.grid.mapWithIndex {
//         (row, i) =>
//           row.zip(fa.grid(i)).map {
//             case (f, a) =>
//               f(a)
//           }
//       }
//       FocusedGrid(ff.focus, newGrid)
//     }
//   }
// }

object Conway {
  import FocusedGrid._

  def getAt(fg: FocusedGrid[Int], point: Tuple2[Int, Int]): Int =
    val row = if (point._1 >= 0) point._1 % fg.grid.size else ((point._1 % fg.grid.size) + fg.grid.size)
    val col = if (point._2 >= 0) point._2 % fg.grid(0).size else ((point._2 % fg.grid(0).size) + fg.grid(0).size)
    val cols = fg.grid(row)
    cols(col)

  // Get the sum of the values around the focus
  def localSum(fg: FocusedGrid[Int]): Int =
    val points = List(-1, 0, 1)
    points.map2(points)((_, _))
      .filter {
        case (0, 0) => false
        case _      => true
      }
      .map(coord => getAt(fg, coord |+| fg.focus))
      .sum

  val blinker = Vector(
    Vector[Int](0, 1, 1, 1, 0),
    Vector[Int](0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0),
    Vector[Int](0, 1, 1, 1, 0),
    Vector[Int](0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0),
    Vector[Int](0, 1, 1, 1, 0),
    Vector[Int](0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0)
  )

  val glider = Vector(
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  val dieHard = Vector(
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  def conwayStep(fg: FocusedGrid[Int]): Int = {
    val liveNeighbours = localSum(fg)
    val live = getAt(fg, fg.focus)

    if (live == 1) {
      if (liveNeighbours >= 2 && liveNeighbours <= 3) 1 else 0
    } else {
      if (liveNeighbours == 3) 1 else 0
    }
  }

  // Convert the digits of the FocusedGrid to more pleasing characters
  def prettify(i: Int): Char =
    i match {
      case 1 => 0x2593.toChar
      case 0 => 0x2591.toChar
    }

  // Ansi code to move the cursor up n lines in the terminal
  def ansiMoveUp(n: Int) = s"\u001b[${n}A"

  def animate(start: FocusedGrid[Int], steps: Int): Unit = {
    println(start.map(a => prettify(a)).show)

    Thread.sleep(40)

    if (steps > 0) {
      print(ansiMoveUp(start.grid.size))
      animate(start.coflatMap(conwayStep), steps - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    // Simple blinker example
    //val b = FocusedGrid((0,0), blinker)

    // Three gliders flying together
    val b = examples.FocusedGrid.FocusedGrid((0, 0), glider)

    // Diehard takes 130 steps to stabilize
    //val b = FocusedGrid((0,0), dieHard)

    animate(b, 135)
  }
}
