package duct.examples

import org.duct.typeclasses.semigroup.{given,*}

object HelloDuct extends App:

  val e1 = List("Hello", ",", " ", "this")
  val e2 = List(" ", "is", " ", "an", " ", "example")
  val e3 = List(" ", "of", " ", "semigroup", " ")
  val e4 = List("combination")

  println(e1.combine(e2))


