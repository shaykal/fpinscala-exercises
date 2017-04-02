package main.scala

import scala.util.matching.Regex

/**
  * Created by Neshume on 25-Mar-17.
  */
trait Parsers[Parser[+_]] { self =>
  // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  /*
   * A default `succeed` implementation in terms of `string` and `map`.
   * We leave `succeed` abstract, since `map` is defined below in terms of
   * `flatMap` and `succeed`, which would be a circular definition! But we include
   * the definition here in case implementations wish to use it
   * (say if they provide a custom implementation of `map`, breaking the cycle)
   */
  def defaultSucceed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    //succeed(f(run(a)))
    flatMap(a)(b => succeed(f(b)))

  def many[A](p: Parser[A]): Parser[List[A]]

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    // use `self` to explicitly disambiguate reference to the `or` method on the `trait`
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    // exercise 9.3
    def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _) or succeed(List())

    // exercise 9.1
    //def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
      //succeed(f(run(product(p, p2))))

    def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = ???
        //flatMap(p)(a => map(p2)(b => (a,b)))
      //flatMap(p)(a => map(p2)(b => (a,b)))

    def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
      //product(p, p2) map (f.tupled)
      flatMap(p)((a: A) => flatMap(p2)((b: B) => succeed(f(a,b))))


    def **[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)

    def many1[A](p: Parser[A]): Parser[List[A]] =
      map2(p, self.many(p))(_ :: _)

    // exercise 9.4
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
      if (n <= 0) succeed(List())
      else map2(p, listOfN(n-1, p))(_ :: _)
    }

    def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

    // exercise 9.6
    def parseNumOfChar(): ((Nothing) => Parser[Nothing]) => Parser[Nothing] =
      for {
        digit: String <- "[0-9]+".r
        n = digit.toInt // we really should catch exceptions thrown by toInt and convert to parse failure
        _ <- listOfN(n, char('a'))
      } yield n


  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
     ??? // forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

  case class ParseError() // (stack: List[(Location,String)]


}