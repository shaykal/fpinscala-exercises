import scala.util.Either

object MyOption {

  // exercise 4.1
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
      case (a: Some[A]) => Some(f(a))
      case _ => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      case (a: Some[A]) => f(a)
      case _ => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case Some(a) => this
      case None => ob
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) => if (f(a)) Some(a) else None
      case None => None
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  // exercise 4.2
  object Option {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    // exercise 4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a.flatMap(x =>
        b.map(y =>
          f(x, y)
        ))
    }

    // exercise 4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight[Option[List[A]]](Some(Nil))((oa, ob) => map2(oa, ob)(_ :: _))

    // a match {
    //   case Nil => Some(Nil)
    //   case h :: t => h.flatMap(vh => sequence(t).map(l => vh :: l))
    // }

    def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      //case x::xs =>  sequence(xs).map((element: List[A]) => x.getOrElse(x) +: element)
    }

    val seq1 = sequence(List(Some(1), Some(2), Some(4)))

    //List(Some(1), Some(2), Some(4)) => Some(1,2,4)

    // exercise 4.5
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      //sequence(a.map(item => f(item)))
      a.foldRight[Option[List[B]]](Some(Nil))((oa: A, olb: Option[List[B]]) => olb.flatMap((ob: List[B]) => f(oa).map(b => b +: ob)))
    }

  }

  // exercise 4.6
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      //(a: A) => try { Right(f(a))} catch {case e: Exception => Left(e)}
      case Left(e) => Left(e)
      case Right(r) => Right(f(r))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(r) => f(r)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => b
      case Right(r) => Right(r)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
      case Right(a) => Right(f(a, b))
      case Left(e) => Left(e)
      // this.flatMap(a => b.map(bb => f(a, bb)))
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  // exercise 4.7
  object Either {

    def Try[A, E](a: => A) : Either[Exception ,A] =
      try (Right(a)) catch {case e: Exception => Left(e)}

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      es.foldRight[Either[E, List[A]]](Right(Nil))((ea: Either[E, A], eb: Either[E, List[A]]) => ea.flatMap(eaa => eb.map(ebb => eaa::ebb)))
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      as.foldRight[Either[E, List[B]]](Right(Nil))((a: A, eb: Either[E, List[B]]) => f(a).map2(eb)(_ :: _) )
    }

  }
}