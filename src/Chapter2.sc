object MyModule {

  // exercise 2.2 ------------------
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def isSrotedRec(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else isSrotedRec(n + 1)
    }

    isSrotedRec(0)
  }

  val values: Array[Int] = Array(1, 2, 3, 4, 16)
  val orderedInt: (Int, Int) => Boolean = (x, y) => x < y

  isSorted(values, orderedInt)

  val valuesStr: Array[String] = Array("hello", "world", "its", "an", "beautiful", "day")
  val valuesStr2: Array[String] = Array("A", "its", "day", "hello", "world", "beautiful")
  val orderedStr: (String, String) => Boolean = (x1: String, y1: String) => x1.length <= y1.length


  isSorted(valuesStr, orderedStr)


  // exercise 2.3

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  // exercise 2.4

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // exercise 2/5

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}