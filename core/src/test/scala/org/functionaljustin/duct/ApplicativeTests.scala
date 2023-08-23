import org.functionaljustin.duct.typeclasses.applicative.{given,*}

class ApplicativeSuite extends munit.FunSuite {
  test("map2 Applicative with list same types") {
    val input1 = List(1,2,3)
    val input2 = List(4,5,6)
    assertEquals(input1.map2(input2){
      case (a,b) => 
        a + b
    }, List(5,6,7,6,7,8,7,8,9))
  }
  test("map2 Applicative with list different types") {
    val input1 = List(1,2,3)
    val input2 = List('a','b','c')
    assertEquals(input1.map2(input2){
      case (a,b) => 
       s"$a and $b"
    }, List(
      "1 and a",
      "1 and b",
      "1 and c",
      "2 and a",
      "2 and b",
      "2 and c",
      "3 and a",
      "3 and b",
      "3 and c"))
  }
}
