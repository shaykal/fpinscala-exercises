
//import main.scala.Monad
//import main.scala.Monad._
1+3

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    ??? //apply(map(fa)(f.curried))(fb)

  def unit[A](a: => A): F[A]

  // exercise 12.2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((a: (A) => B, b: A) => a(b))

  // derived combinators
  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    map2(fa, unit(()))((a, _) => f(a))
    //apply(unit(f))(fa)
  }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  // exercise 12.1
  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    //lma.foldRight[F[List[A]]](unit(List()))((a: F[A], b: F[List[A]]) => map2(a, b)(_ :: _))
    traverse(lma)(identity)
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    if (n <= 0) unit(List())
    else {
      map2(ma, replicateM(n - 1, ma))(_ :: _)
    }
  }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a: A, b: B) => (a, b))


  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] = {
    ??? //apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }

  def map4[A, B, C, D, E](fa: F[A],
                          fb: F[B],
                          fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] =
    ??? //apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  // exercise 12.8
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A) = (self.unit(a), G.unit(a))

      def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))

      override def map[A, B](fa: (F[A], G[A]))(f: (A) => B): (F[B], G[B]) = ???
    }
  }

  // exercise 12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa.foldLeft[F[Map[K, V]]](unit(Map())) {
      case (acc, (k, fv)) => map2(acc, fv)((m, v) => m + (k -> v))
    }
  }
}

1 + 3

// exercise 12.5
object MyEitherMon {
/*
  import main.scala.Monad._
  import main.scala.Monad

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)

      def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]) = eea match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }
    }
    */
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

// exercise 12.6
def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = {
  new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A) : Validation[E, A]= Success(a)

    override def map2[A,B,C](fa: Validation[E,A], fb: Validation[E,B])(f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (Success(valA), Success(valB)) => Success(f(valA, valB))
        case (Failure(headA, tailA), Failure(headB, tailB)) => Failure(headA, tailA ++ Vector(headB) ++ tailB)
        case (e@Failure(_, _), _) => e
        case (_, e@Failure(_, _)) => e
      }
    }
  }
}


