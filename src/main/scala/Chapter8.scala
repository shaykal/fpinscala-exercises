package main.scala

import main.scala.workingExamples.Prop.{MaxSize, Result, TestCases}

case class Gen[A](sample: State[RNG,A]) {

  // exercise 8.6
  def flatMap[B](f: A => workingExamples.Gen[B]): workingExamples.Gen[B] = {
    workingExamples.Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Int): workingExamples.Gen[List[A]] =
   ??? // listOfN()

  def listOfN(size: Gen[Int]): workingExamples.Gen[List[A]] = {
    size flatMap (n => this.listOfN(n))
  }

  // exercise 8.7
  def union[A](g1: workingExamples.Gen[A], g2: workingExamples.Gen[A]): workingExamples.Gen[A] = {
    workingExamples.Gen.boolean.flatMap(b => if (b) g1 else g2)
  }

  // exercise 8.8
  def weighted[A](g1: (workingExamples.Gen[A],Double), g2: (workingExamples.Gen[A],Double)): workingExamples.Gen[A] = {
    val total = g1._2 + g2._2
    workingExamples.Gen.choose(0, total.toInt).flatMap((index: Int) => if (index < g1._2 / total) g1._1 else g2._1)
  }
}


case class Prop(run: (MaxSize,TestCases,RNG) => Result)

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
/*
  case object Proved extends Result {
    def isFalsified = false
  }
*/


  def randomStream[A](g: workingExamples.Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
/*
  def forAll[A](as: workingExamples.Gen[A])(f: A => Boolean): workingExamples.Prop = workingExamples.Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }
  */
}


object Gen {
  // exercise 8.4
  def choose(start: Int, stopExclusive: Int): State[RNG, MaxSize] = {
    State(StateObj.nonNegativeInt).map((n: Int) => start + n % (stopExclusive - start))
  }

  // exercise 8.5
  def unit[A](a: => A): workingExamples.Gen[A] =
    workingExamples.Gen(State(StateObj.unit(a)))

  def listOfN[A](n: Int, g: workingExamples.Gen[A]): workingExamples.Gen[List[A]] = {
    workingExamples.Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def boolean: workingExamples.Gen[Boolean] = {
    val num = choose(0, 2)
    //val test: (RNG) => (Int, RNG) = num.sample.run
    //workingExamples.Gen(State(test).map((elem: Int) => if (elem == 0) false else true))
    ???
  }
}