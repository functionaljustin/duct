package org.justinhj.duct
import org.justinhj.duct.datatypes.{LazyList,#::}
import org.justinhj.duct.typeclasses.monad.lazyListMonad

// Testing for code developed in the video https://youtu.be/laB15gG5bjY Ep 17: The Magic of LazyLists

import scala.collection.immutable.{LazyList as StdLazyList}
import org.justinhj.duct.datatypes.LazyList.apply

class LazyListSuite extends munit.FunSuite{
  test("apply can build lists, and toList works") {
    val input1 = LazyList(1,2,3)
    val input2 = LazyList("a","b","c")
    assertEquals(input1.toList, List(1,2,3))
    assertEquals(input2.toList, List("a","b","c"))

    val empty = LazyList()
    assertEquals(empty.toList, List.empty)
  }
  test("head works and tail is not evaluated") {
    val lazyBoom = 1 #:: {throw new Exception("Boom"); LazyList.empty}
    assertEquals(lazyBoom.head, 1)
  }
  test("head is not evaluated") {
    val lazyBoom = LazyList.cons({throw new Exception("Boom"); 1}, LazyList.empty)
  }
  test("Duct foldRight can terminate early") {
    val fish = "salmon" #:: "shark" #:: "tuna" #:: "moray" #:: "goldfish" #:: "eel" 
      #:: {throw new Exception("Boom"); LazyList.empty}
    val result = fish.foldRight(List("barracuda")){
            (a, acc) => 
                if a == "tuna" then 
                    List("tuna")
                else
                    acc :+ a
        }
    assertEquals(result, List("tuna","shark","salmon"))
  }
  test("Scala stdlib foldRight can not terminate early") {
    val fish = "salmon" #:: "shark" #:: "tuna" #:: "moray" #:: "goldfish" #:: "eel" 
      #:: {throw new Exception("Boom"); StdLazyList.empty}

    intercept[Exception] {
      fish.foldRight(List("barracuda")){
              (a, acc) => 
                  if a == "tuna" then 
                      List("tuna")
                  else
                      acc :+ a
          }
    }
  }
  test("Stack safety in various operations") {
    // This test is from test/files/run/streams.scala in Scala branch Scala-2.7-diverged
    val size = 100000
    try {
      // Stack overflow tests from Scala 2.7 Stream tests                                                                                                             
      LazyList.from(1).drop(size)
      LazyList.from(1).filter(_ > size).take(5)
      LazyList.from(1).take(size).forall(_ >= 0)
      LazyList.from(1).exists(_ > size) 
      LazyList.from(1).take(size).foldLeft(0)(_ + _)
    } catch {
      StackOverflowError =>
        fail(s"Should not cause stack overflow")
    }
  }
  test("++ works") {
    val appended = LazyList.repeat(1).take(5) ++ LazyList.repeat(2).take(5)
    assertEquals(appended.take(3).toList, List(1,1,1))
    assertEquals(appended.take(6).toList, List(1,1,1,1,1,2))
    // Verify laziness of right hand side
    // TODO left hand side is not lazy
    // TODO right hand side booms if you reach the tail (i.e., take 3 elements in last line below)
    val tailBomb1 = 1 #:: 2 #:: {throw new Exception("Boom!"); LazyList.empty}
    val prependedBeforeBomb = (1 #:: LazyList.empty) ++ tailBomb1
    assertEquals(prependedBeforeBomb.take(2).toList, List(1,1))
  }
  test("flatMap") {
    def reps(n: Int): LazyList[Int] = {
      LazyList.repeat(n).take(n)
    }
    assertEquals(LazyList(1,2,3).flatMap(n => reps(n)).toList, LazyList(1,2,2,3,3,3).toList)
  }
}