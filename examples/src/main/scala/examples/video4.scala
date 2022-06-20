// Functional Justin https://youtu.be/B2mTHpxw9JI
// Ep 4: Scala 3 type classes
// Here I'm just demoing the Numeric typeclass developed using Scala 3
object Video4 extends App:
    val v1 = summon[Numeric[String]].square("ab")
    println(v1)
    
    val v2 = "abcd" * "efgh"
    println(v2)
    
    val v3 = 10.square
    println(v3)