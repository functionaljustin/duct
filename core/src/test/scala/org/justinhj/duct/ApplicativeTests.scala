import org.justinhj.duct.typeclasses.applicative.{given,*}

class ApplicativeSuite extends munit.FunSuite {
  test("map2 Applicative with list") {
    val input1 = List(1,2,3)
    val input2 = List(4,5,6)
    assertEquals(input1.map2(input2){
      case (a,b) => 
        a + b
    }, List(5,6,7,6,7,8,7,8,9))
  }
}
