//import main.scala.Monad
//import main.scala.Monad._

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
     ??? //apply(map(fa)(f.curried))(fb)

  def unit[A](a: => A): F[A]

  // exercise 12.2
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((a: (A) => B, b: A) => a(b))

  // derived combinators
  override def map[A,B](fa: F[A])(f: A => B): F[B] = {
    map2(fa, unit(()))((a, _) => f(a))
    //apply(unit(f))(fa)
  }

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  // exercise 12.1
  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    //lma.foldRight[F[List[A]]](unit(List()))((a: F[A], b: F[List[A]]) => map2(a, b)(_ :: _))
    traverse(lma)(identity)
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    if (n <= 0) unit(List())
    else {
      map2(ma, replicateM(n-1, ma))(_ :: _)
    }
  }

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((a: A, b: B) => (a, b))


  def map3[A,B,C,D](fa: F[A],
                    fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] = {
    ??? //apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }

  def map4[A,B,C,D,E](fa: F[A],
                      fb: F[B],
                      fc: F[C],
                      fd: F[D])(f: (A, B, C, D) => E): F[E] =
  ??? //apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
}

1 + 4

// exercise 12.5
//object MyEitherMon {
//  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f]
//}