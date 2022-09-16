package org.justinhj.duct
import org.justinhj.duct.datatypes.{LazyList,#::}

// Testing for code developed in the video https://youtu.be/laB15gG5bjY Ep 17: The Magic of LazyLists

import scala.collection.immutable.{LazyList as StdLazyList}

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
}
