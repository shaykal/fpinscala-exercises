import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll


trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  // exercise 10.1
  val intAddition: Monoid[Int] = {
    new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2

      override def zero: Int = 0
    }
  }

  val intMultiplication: Monoid[Int] = {
    new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 * a2

      override def zero = 1
    }
  }

  val booleanOr: Monoid[Boolean] = {
    new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

      override def zero = true
    }
  }

  val booleanAnd: Monoid[Boolean] = {
    new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

      override def zero = false
    }
  }

  // exercise 10.2
  def optionMonoid[A]: Monoid[Option[A]] = {
    new Monoid[Option[A]] {
      override def op(a1: Option[A], a2: Option[A]): Option[A] =
        a1.orElse(a2)

      /*
      override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 match {
        case orig @ Some(_) => orig
        case None => a2 match {
          case orig2 @ Some(_) => orig2
          case _ => _
        }
      }
      */
      override def zero = None
    }
  }

  // exercise 10.3
  def endoMonoid[A]: Monoid[A => A] = {
    new Monoid[(A) => A] {
      override def op(a1: (A) => A, a2: (A) => A): (A) => A = (a: A) => a1(a2(a))

      override def zero: (A) => A = (a: A) => a
    }
  }

  // exercise 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val associativeLaw: Prop = forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3))

//    forAll(gen) {(a1: A, a2: A, a3: A) =>
//    m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3))
//  }

    val zeroLaw: Prop = forAll(gen)((a1: A) =>
      m.op(m.zero, a1) == a1 && m.op(a1, m.zero) == a1
    )

    associativeLaw && zeroLaw
  }

  // exercise 10.5
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldRight(m.zero)((a: A, b: B) => m.op(f(a), b))
  }
}