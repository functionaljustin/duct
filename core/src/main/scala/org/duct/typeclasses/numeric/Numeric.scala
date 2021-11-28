package org.justinhj.typeclasses.numeric

// Extension methods allow one to add methods to a type after the type is defined
  // Which is the essence of type classes!

  // Numeric type class in Scala 3
  trait Numeric[T] {
    def add(a: T, b: T): T
    def mul(a: T, b: T): T
    def div(a: T, b: T): T
    def sub(a: T, b: T): T
    def isZero(a: T): Boolean 
    
    extension (a: T) {
      def +(b: T): T = add(a, b)
      def -(b: T): T = sub(a, b)

      def *(b: T): T = mul(a, b)
      def /(b: T): T = div(a, b)

      def square: T = mul(a, a)
    }
  }

  given Numeric[Int] with {
    def add(a: Int, b: Int): Int = a + b
    def sub(a: Int, b: Int): Int = a - b

    def mul(a: Int, b: Int): Int = a * b
    def div(a: Int, b: Int): Int = a / b
    
    def isZero(a: Int) = a == 0
  }

  given Numeric[String] with {
    // Adding strings is appending
    def add(a: String, b: String): String = a + b
    
    // Subtract string b from a (kinda)
    def sub(a: String, b: String): String = {
      val newSize = a.size - b.size
      a.substring(0, newSize)
    }

    def div(a: String, b: String): String = {
      val newSize = a.size / b.size
      a.substring(0, newSize)
    }

    def mul(a: String, b: String): String = {
      for (
        as <- a;
        bs <- b;
        s <- as.toString ++ bs.toString
      ) yield s
    }
    
    def isZero(a: String) = a.size == 0
  }

