
object MyList {

  // exercise 3.2 --------------
  def tail[T](xs : List[T]) : List[T] = xs match {
    case Nil => Nil
    case x::xs => xs
  }

  // exercise 3.3 --------------
  def replaceHead[T](newElement: T, xs: List[T]) : List[T] = xs match {
    case Nil => List(newElement)
    case x::xs => newElement +: xs
  }

  val list10 = List(1,2,3,4,5,6,7,8,9,10)
  replaceHead(200, list10)

  // exrecise 3.4 ---------------
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _) => Nil
    case (newList, 0) => newList
    case (x::xs, num) => drop(xs, n-1)
  }

  drop(list10, 3)
  drop(list10, 5)

  // exercise 3.5 -----------------
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case x::xs =>
      if(f(x)) dropWhile(xs, f)
      else x::xs
  }

  val predicateLT7 : Int => Boolean = x => x < 7
  dropWhile(list10, predicateLT7)
  val predicateGT4 : Int => Boolean = x => x > 4
  dropWhile(list10, predicateGT4)

  // exercise 3.6 ---------------------------
  def init[A](l: List[A]): List[A] = {
    def initRec[A](acc: List[A], l: List[A]): List[A] = l match {
      case Nil => acc
      case x::Nil => acc
      case x::xs => initRec(acc :+ x, xs)
    }
    initRec(Nil, l)
  }

  init(list10)


  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case x::xs => f(x, foldRight(xs, z)(f))
    }

  // exercise 3.8 ----------
  val testConsNilFolderRight = foldRight(List(1,2,3), Nil:List[Int])((x,y) => x::y)

  // exercise 3.9 ==============
  def length[A](as: List[A]): Int = as match {
    case Nil => 0
    case x::xs => 1 + length(xs) //foldRight(as, 0)()
  }

  length((list10))

  def length2[A](as: List[A]): Int = {
    foldRight(as, 0)((x,y) => 1 + length2(as.tail))
  }

  val length3 = length2(list10)


// exercise 3.10 --------------
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case x::Nil => f(z, x)
    case x::xs => foldLeft(xs, f(z, x))(f)
  }

  // exercise 3.11 -------------------
  def sumWithFoldLeft(values : List[Int]) : Int = {
    foldLeft(values, 0)(_ + _)
  }

  def productWithFoldLeft(values : List[Int]) : Int = {
    foldLeft(values, 1)(_ * _)
  }


  def lengthWithFoldLeft(values: List[Int]) : Int = {
    foldLeft(values, 0)((a,b) => a + 1)
  }

  val sumOf10 = sumWithFoldLeft(list10)
  val productOf10 = productWithFoldLeft(list10)
  val lengthOf10 = lengthWithFoldLeft(list10)


  // exercise 3.12 --------------
  def reverse[A](values : List[A]) : List[A] = values match {
    case Nil => Nil
    case x::Nil => List(x)
    case x::y::Nil => List(y,x)
    case x::y::xs => foldRight(reverse(xs), List(y,x))((a: A, b: List[A]) => a +: b)
  }

  reverse(list10)

  // exercise 3.13 -----------------

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)


  // exercise 3.14 ------------------

  def append[A](a1 : List[A], a2: List[A]) : List[A] = {
    foldRight(a1, a2)((a: A, listOfA: List[A]) => a +: listOfA)
  }

  append(list10, List(11,12,13))


  // exercise 3.15 ----------------
  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, List[A]())((a: List[A], b: List[A]) => append(a, b))
  }

  concat(List(List(1,2,3), List(4,5,6), List(7,8,9))) //  -> List(1,2,3,4,6,7,8,9)


  // exercise 3.16
  def addBy1(values: List[Int]) : List[Int] = values match {
    case Nil => Nil
    case x::xs => (x+1) +: addBy1(xs)
  }

  addBy1(list10)

  def addBy1_v2(values: List[Int]) : List[Int] = {
    foldRight(values, List[Int]())((a: Int, b: List[Int]) => (a+1) +: b)
  }

  addBy1_v2(list10)

  // exercise 3.17
  def convertToString(values: List[Double]) : List[String] = {
    foldRight(values, List[String]())((a: Double, b: List[String]) => (a.toString) +: b)
  }

  convertToString(List(1d, 2d, 3d, 4d))

  // exercise 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, List[B]())((a: A, b: List[B]) => f(a) +: b)
  }

  map(list10)((x: Int) => x * 2)

    // exercise 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      foldRight(as, List[A]())((a: A, b: List[A]) => if(f(a)) a +: b else b)
    }

  filter(list10)((x: Int) => x % 2 == 0)

  // exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List[B]())((a: A, b: List[B]) => f(a) ++ b)

  }

  flatMap(List(1,2,3))(i => List(i,i))

  // exercise 3.21
  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((a: A) => if (f(a)) List(a) else Nil)
  }

  filterByFlatMap(list10)((x: Int) => x % 2 == 0)

  // exercise 3.22
  def addingValuesLists(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, Nil) => Nil
    case (Nil, b) => b
    case (a, Nil) => a
    case (a, b) => (a.head + b.head) +: addingValuesLists(a.tail, b.tail)
  }

  addingValuesLists(List(1,2,3), List(4,5,6))

  def zipWith[A](a1: List[A], a2: List[A])(f: (A,A) => A) : List[A] = (a1,a2) match {
    case (Nil, Nil) => Nil
    case (Nil, b) => b
    case (a, Nil) => a
    case (a, b) => (f(a.head, b.head)) +: zipWith(a.tail, b.tail)(f)
  }

  zipWith(List(1,2,3), List(4,5,6))((a: Int, b: Int) => a + b)
  zipWith(List(1,2,3), List(4,5,6))(_ + _)

  // exercise 3.24
  @annotation.tailrec
  def startsWith[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (h1 :: t1, h2 :: t2) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case _ if startsWith(sup, sub) => true
    case (_ :: t) => hasSubsequence(t, sub)
  }

  hasSubsequence(list10, List(4))
  hasSubsequence(list10, List(11))
  hasSubsequence(list10, List(1,2,3))
  hasSubsequence(list10, List(4,5,6))
  hasSubsequence(list10, List(4,5,7, 8))

}



