import main.scala.{Parsers, State}


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // exercise 11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldRight[F[List[A]]](unit(List()))((a: F[A], b: F[List[A]]) => map2(a, b)(_ :: _))
  }
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = {
    la.foldRight[F[List[B]]](unit(List()))((a: A, b: F[List[B]]) => map2(f(a), b)(_ :: _))
  }

  // exercise 11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    if (n <= 0) unit(List())
    else {
      map2(ma, replicateM(n-1, ma))(_ :: _)
    }
  }

  // exercise 11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM(t)(f)
        else map(filterM(t)(f))(h :: _))
    }
  }

  // exercise 11.7
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    (a: A) => flatMap(f(a))(g)

  // exercise 11.8
  def flatMapViaCompose[A,B](ma: F[A])(f: A => F[B]) : F[B] = {
    compose((_:Unit) => ma, f)()
  }

  // exercise 11.12
  def join[A](mma: F[F[A]]): F[A] = {
    flatMap(mma)((ma: F[A]) => ma)
  }

  // exercise 11.13
  def composeViaJoinAndMap[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
    (a: A) => join(map(f(a))(g))
  }

  def flatMapViaJoinAndMap[A,B](ma: F[A])(f: A => F[B]): F[B] = {
    join(map(ma)(f))
  }
}

object Monad {
  //val genMonad = new Monad[Gen] {
   // def unit[A](a: => A): Gen[A] = Gen.unit(a)
    //def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
     // ma flatMap f
 // }
  // exercise 11.1
  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] =
      ma.flatMap(entry => f(entry))
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] =
      ma.flatMap(f)
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  def parsersMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
    override def unit[A](a: => A): P[A] = p.succeed(a)
    override def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] =
      p.flatMap(ma)(f)
  }
/*
  def parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    //override def flatMap[A, B](ma: p.Par[A])(f: (A) => p.Par[B]): p.Par[B] =
     // p.Par.flatMap(ma)(f)
    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }
*/

  // exercise 11.2
  def stateMonda[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }
}


// exercise 11.17
case class Id[A](value: A) {
  def map[B](f: A => B) : Id[B] =
    Id(f(value))

  def flatMap[B](f: A => Id[B]) : Id[B] =
    f(value)
}

object Id {
  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] =
      ma.flatMap(f)
  }

}