import org.justinhj.duct.datatypes.NonEmptyList

class NonEmptyListSuite extends munit.FunSuite {
  test("create various lengths and types of NonEmptyList") {
    val oneList = NonEmptyList(1)
    val twoList = NonEmptyList(2,3)
    val threeList = NonEmptyList("four", "five", "six")
  }
  test("append of lists of the same type") {
    val oneList = NonEmptyList(1)
    val twoList = NonEmptyList(2,3)
    val onetwoList = oneList.append(twoList)
    assertEquals(onetwoList.toList, List(1,2,3))
  }
  test("append of lists of different types") {
    val oneList = NonEmptyList(1)
    val twoList = NonEmptyList(2,3)
    val threeList = NonEmptyList("four", "five", "six")
    val onetwoList = oneList.append(twoList)
    val onetwothreeList = onetwoList.append(threeList)
    assertEquals(onetwothreeList.toList, List(1,2,3,"four","five","six"))
  }
  test("toList") {
    assertEquals(NonEmptyList(1.0,2.0,3.0).toList, List(1.0,2.0,3.0)) 
  }
}
