import org.justinhj.duct.datatypes.NonEmptyList

class NonEmptyListSuite extends munit.FunSuite {
  test("create lists of various arities") {
    val oneList = NonEmptyList(1)
    val twoList = NonEmptyList(2,3)
    val threeList = NonEmptyList("four", "five", "six")

    val onetwoList = oneList.append(twoList)
    // val onetwothreeList = onetwoList.append(threeList)
    assertEquals(onetwoList.toList, List(1,2,3))
  }
}