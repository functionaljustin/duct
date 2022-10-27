package org.justinhj.duct.datatypes

import org.justinhj.duct.datatypes.NonEmptyList

class NonEmptyListSuite extends munit.FunSuite {
  test("create various lengths and types of NonEmptyList") {
    // As expected creating an empty NonEmptyList is not allowed
    // and causes a compile error
    // val emptyList = NonEmptyList()
    val oneList = NonEmptyList(1)
    val twoList = NonEmptyList(2,3)
    val threeList = NonEmptyList("four", "five", "six")
    assertEquals(oneList.toList, List(1))
    assertEquals(twoList.toList, List(2,3))
    assertEquals(threeList.toList, List("four", "five", "six"))
  }
  test("append of lists of the same type") {
    val oneList = NonEmptyList(1)
    val twoList = NonEmptyList(2,3)
    val onetwoList = oneList.append(twoList)
    assertEquals(onetwoList.toList, List(1,2,3))
  }
  test("toList") {
    assertEquals(NonEmptyList(1.0,2.0,3.0).toList, List(1.0,2.0,3.0)) 
  }
  test("fromSeq when empty seq") {
    val empty = List.empty[Int]
    assertEquals(NonEmptyList.fromSeq(empty),None) 
  }
  test("NonEmptyList is a Functor") {
    val someThings = List(1,2,3,4,5)
    val someThingsNel = NonEmptyList(1,2,3,4,5)
    assertEquals(someThingsNel.map(identity),someThingsNel)
    assertEquals(someThingsNel.map(a => (a*10).toString), NonEmptyList("10","20","30","40","50"))
  }
  test("fromSeq when not empty seq") {
    val someThings = List(1,2,3,4,5)
    val someThingsNel = NonEmptyList(1,2,3,4,5)
    assertEquals(NonEmptyList.fromSeq(someThings),Some(someThingsNel))
  }
  test("tails of various sized NonEmptyLists") {
    assertEquals(NonEmptyList(1).tails, NonEmptyList(NonEmptyList(1)))
    assertEquals(NonEmptyList(1,2).tails, NonEmptyList(
      NonEmptyList(1,2), NonEmptyList(2)
      ))
    assertEquals(NonEmptyList(1,2,3).tails, NonEmptyList(
      NonEmptyList(1,2,3), NonEmptyList(2,3), NonEmptyList(3)
      ))
  }
}
