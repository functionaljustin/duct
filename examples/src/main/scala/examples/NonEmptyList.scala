import org.justinhj.duct.datatypes.NonEmptyList

object NonEmptyListDemo extends App:

  // Basic usage
  val list1 = List(1,2,3)
  val nel1 = NonEmptyList.fromSeq(list1).get

  val list2 = List(4,5,6)
  val nel2 = NonEmptyList.fromSeq(list2).get

  val appended = nel1.append(nel2)
  println(appended)

  // Construct from var args ...
  val constructed = NonEmptyList(1,2,3,4)
