import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll


trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

1 + 5

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

  // exercise 10.6
  def foldRightViaMap[A,B](as: List[A])(z: B)(f: (A, B) => B) : B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  // exercise 10.7
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length == 0)
      m.zero
    else if (as.length == 1)
      f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  // exercise 10.9
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // Our monoid tracks the minimum and maximum element seen so far
    // as well as whether the elements are so far ordered.
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match {
          // The ranges should not overlap if the sequence is ordered.
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero = None
    }
    // The empty sequence is ordered, and each element by itself is ordered.
    foldMapV(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }


  // exercise 10.16
  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A,B)] = {
    new Monoid[(A,B)] {
      override def op(x: (A, B), y: (A, B)): (A, B) =
        (a.op(x._1, y._1), b.op(x._2, y._2))

      override def zero: (A, B) = (a.zero, b.zero)
    }
  }

  // exercise 10.17
  def functionMonoid[A,B](b: Monoid[B]): Monoid[A => B] = {
    new Monoid[(A) => B] {
      override def op(f: (A) => B, g: (A) => B): (A) => B = {
        (a : A) => b.op(f(a), g(a))
      }

      override def zero: (A) => B = (a: A) => b.zero
    }
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }

    }
  // exercise 10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))
  }

}
val test123 = 1 + 3
test123
/*
import Monoid._
foldMap(List("first","second"), intMultiplication)((a: String) => a.length)
monoidLaws(intAddition, Gen.choose(0, 100)).check

val optionGenerator : Gen[Option[Int]] =
  Gen.frequency[Option[Int]](
    (7, Some(1)),
    (10, None)
  )

monoidLaws(optionMonoid[Int], optionGenerator).check
*/

// exercise 10.12
trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // exercise 10.15
  def toList[A](fa: F[A]): List[A] = {
    foldRight(fa)(List[A]())((a: A, b: List[A]) => a :: b)
  }
}


object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B =
    as.foldRight(mb.zero)((a: A, b: B) => mb.op(f(a), b))
}

object FoldableIndexedSeq extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B =
    as.foldRight(mb.zero)((a: A, b: B) => mb.op(f(a), b))
}

object FoldableStream extends Foldable[Stream] {
  def foldRight[A,B](as: Stream[A])(z: B)(f: (A,B) => B): B =
    as.foldRight(z)(f)

  def foldLeft[A,B](as: Stream[A])(z: B)(f: (B,A) => B): B =
    as.foldLeft(z)(f)

  def foldMap[A,B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldRight(mb.zero)((a: A, b: B) => mb.op(f(a), b))
}

// exercise 10.13
//sealed trait Tree[+A]
//case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
//case object Leaf(value: A) extends Tree[A]

// exercise 10.14
/*
object FoldableOption extends Foldable[Option] {
  def foldRight[A,B](as: Option[A])(z: B)(f: (A,B) => B): B = as match {
    case Some(value) => f(value, z)
    case None => z
  }


  def foldLeft[A,B](as: Option[A])(z: B)(f: (B,A) => B): B = as match {
    case Some(value) => f(value, z)
    case None => z
  }

  def foldMap[A,B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Some(value) => mb.op(f(value), mb.zero)
    case None => mb.zero
  }
}
*/
1 + 4
import Monoid._
val m: Monoid[(Int, Int)] = productMonoid(intAddition, intAddition)
val p = ListFoldable.foldMap(List(1,2,3,4))(a => (a, 1))(m)
