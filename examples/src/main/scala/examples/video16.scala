package examples

import org.justinhj.duct.datatypes.NonEmptyList
import org.justinhj.duct.typeclasses.semigroup.{given}

object Video16 extends App:

    //val nel1: NonEmptyList[String] = NonEmptyList()
    val nel2 = NonEmptyList("justin@email.com")
    val nel3: NonEmptyList[String] = NonEmptyList("justin1@email.com","justin2@email.com","justin3@email.com")

    val appended = nel2 |+| nel3

    println(s"nel2 and nel3 ${appended.toList()}")

    val l1 = List("justin1@email.com","justin2@email.com","justin3@email.com")
    println(NonEmptyList.fromSeq(l1))

    val el = List.empty[String]
    println(NonEmptyList.fromSeq(el))

