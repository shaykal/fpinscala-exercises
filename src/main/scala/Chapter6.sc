trait RNG {
  def nextInt: (Int, RNG)
}



object StateObj {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (num, newRNG) if (num < 0) => (-num-1, newRNG)
    case value => value
  }

  // exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (intVal, newRNG) = nonNegativeInt(rng)
    ((intVal / Int.MaxValue.toDouble + 1), newRNG )
  }

  // exercise 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, newRNG1) = nonNegativeInt(rng)
    val (d, newRNG2) = double(newRNG1)
    ((i, d), newRNG2)

  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (i, newRNG1) = nonNegativeInt(rng)
    val (d, newRNG2) = double(newRNG1)
    ((d, i), newRNG2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, newRNG1) = double(rng)
    val (d2, newRNG2) = double(newRNG1)
    val (d3, newRNG3) = double(newRNG2)
    ((d1, d2, d3), newRNG3)
  }

  // exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match{
    case 0 => (Nil, rng)
    case num => {
      val (i, newRNG) = nonNegativeInt(rng)
      (i :: ints(num - 1)(newRNG)._1, newRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // exercise 6.5
  def double2 : Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  // exercise 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      (f(ra(rng)._1, rb(ra(rng)._2)._1), rb(ra(rng)._2)._2)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(nonNegativeInt, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, nonNegativeInt)

  // exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((a: Rand[A], b: Rand[List[A]]) => map2(a,b)(_ :: _))
  }


  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  // exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1) // We pass the new state along
    }
  }

  // exercise 6.9
  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(i =>
      map(rb)(j =>
        f(i,j)
      ))
  }

}

// exercise 6.10
import State._

type StateType[S,+A] = S => (A,S)

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}


object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight[State[S, List[A]]](unit(List()))((a: State[S, A], b: State[S, List[A]]) => a.map2(b)(_ :: _))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}



// exercise 6.11
sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def interact(input: Input) : Machine = input match {
    case Coin if locked && candies > 0 => new Machine(false, candies, coins + 1)
    case Turn if !locked => new Machine(true, candies - 1, coins)
    case _ => this
  }

}

object CandyDispenser {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(input => modify[Machine](_.interact(input))))
    machine <- get
  } yield (machine.coins, machine.candies)
}




val nonNegatives = StateObj.nonNegativeInt(StateObj.SimpleRNG(1000L))
StateObj.ints(3)(nonNegatives._2)
StateObj.double(nonNegatives._2)

StateObj.double2.apply(nonNegatives._2)

val machine = new Machine(true, 10, 5)
CandyDispenser.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
