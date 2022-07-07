package examples

import org.justinhj.duct.typeclasses.semigroup.{given,*}

object HelloDuct extends App:

  val e1 = List("Hello", ",", " ", "this")
  val e2 = List(" ", "is", " ", "an", " ", "example")
  val e3 = List(" ", "of", " ", "semigroup", " ")
  val e4 = List("combination")

  val combine1 = e1.combine(e2)
  val combine2 = e3.combine(e4)
  val combineAll = combine1.combine(combine2)

  combineAll.foreach {
    s => print(s)
  }
  print("\n")
