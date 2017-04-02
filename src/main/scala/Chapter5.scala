package main.scala

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // exercise 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // exercise 5.2
  def take(n: Int) : Stream[A] = this match {
    case Empty => Stream.empty
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n-1) )
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
  }

  def drop(n: Int) : Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) Stream.cons(h(), t().takeWhile(p)) else Stream.empty
    case _ => Stream.empty
  }

  // exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // exercise 5.5
  def takeWhileViaFold(f : A => Boolean) : Stream[A] = {
    foldRight[Stream[A]](Stream.empty)((a, b) => if (f(a)) Stream.cons(a, b ) else Stream.empty)
  }

  // exercise 5.6
  def headOptionViaFoldRight: Option[A] =
    foldRight[Option[A]](None)((h, t) => Some(h))

  // exercise 5.7
  def map[B](f: A => B) : Stream[B] =
    foldRight[Stream[B]](Stream.empty)((h, t) => Stream.cons(f(h), t))

  def filter(p: A => Boolean) : Stream[A] =
    foldRight[Stream[A]](Stream.empty)((h, t) => if(p(h)) Stream.cons(h, t) else t)

  def append[B >: A](that: => Stream[B]) : Stream[B] =
    foldRight[Stream[B]](that)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]) : Stream[B] =
    foldRight[Stream[B]](Stream.empty)((h, t) => f(h) append t)

  // exercise 5.13
  def mapViaUnfold[B](f: A => B) : Stream[B] = Stream.unfold(this) {
    case Cons(hd, tl) => Some(f(hd()), tl())
    case _ => None
  }

  def takeViaUnfold(n: Int) : Stream[A] = Stream.unfold(this) {
    //Stream.unfold(this)(s => Some(s, ))
    case Cons(hd, tl) if n > 1 => Some(hd(), tl().takeViaUnfold(n-1))
    case Cons(hd, _) if n == 1 => Some(hd(), Stream.empty)
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(hd, tl) => if(p(hd())) Some(hd(), tl()) else None
    case Empty => None
  }

  def zipWithViaUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold(this, s2) {
      case (Cons(hd1,tl1), Cons(hd2,tl2)) => Some(f(hd1(),hd2()), (tl1(), tl2()))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    Stream.unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Stream.empty)))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Stream.empty, t2())))
      case _ => None
    }
  }

  // exercise 5.14
  def startsWith[A](s: Stream[A]): Boolean = {
    this.zipWithViaUnfold(s)((a,b) => a == b).forAll(element => element == true)
  }

  // exercise 5.15
  def tails: Stream[Stream[A]] = {
    Stream.unfold(this) {
      case Empty => None
      case hd @ Cons(_, tl) => Some(hd, tl())
    } append Stream(Empty)
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
}


  //val infiniteCons = Stream.constant

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val test = 5

  val ones: Stream[Int] = cons(1, ones)

  // exercise 5.8
  def constant[A](a: A) : Stream[A] = cons(a, constant(a))

  // exercise 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // exercise 5.10
  val fibs : Stream[Int] = {
    def fibsInner(a: Int, b: Int) : Stream[Int] = {
      cons(a, fibsInner(b, a+b))
    }
    fibsInner(0,1)
  }

  // exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  // exercise 5.12
  def onesByUnfold() : Stream[Int] =
    unfold(1)(_ => Some(1,1))

  def fibsByUnfold() : Stream[Int] = {
    unfold((0,1))(pair => Some(pair._1, (pair._2, pair._1 + pair._2)))
  }

  def constantByUnfold[A](a: A) : Stream[A] = {
    unfold(a)(element => Some(element, element))
  }

  def fromByValue(n: Int) : Stream[Int] = {
    unfold(n)(element => Some(element, element + 1))
  }

}

  Stream.test
  Stream.constant("a").take(7).toList
  Stream.from(10).take(5).toList
  Stream.fibs.take(10).toList

  Stream.fibsByUnfold.take(10).toList
  Stream.onesByUnfold().take(5).toList
  Stream.constantByUnfold("b").take(7).toList
  Stream.fromByValue(10).take(5).toList

  val testMapViaUnfold : List[Int] = Stream.from(10).mapViaUnfold(a => a * 2).take(5).toList
  val testTakeViaUnfold = Stream.from(10).takeViaUnfold(5).toList
  val testTakeWhileViaUnfold = Stream.from(10).takeWhileViaFold(a => a < 20).toList

  val streamFrom10 = Stream.from(10)
  val testStartsWith = Stream(1,2,3,4)
  streamFrom10.startsWith(testStartsWith)

  Stream.from(10).take(5).tails.toList