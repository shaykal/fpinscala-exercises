import java.util.concurrent.TimeUnit
//import java.util.concurrent.{ExecutorService, Future}

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???
}

trait Callable[A] { def call: A }
trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}



type Par[A] = ExecutorService => Future[A]
def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

object Par {
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // def unit[A](a: => A): Par[A]
  // def unit[A](a: => A): Par[A]


  def get[A](a: Par[A]): A = ???

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))


  // exercise 7.1
  //def map2[A](f: IndexedSeq[A] => Par[A], g: IndexedSeq[A] => Par[A])(f: (A,A) => A) : Par[A] = ???

  // exercise 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  // exercise 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](lazyUnit(List()))((a: Par[A], b: Par[List[A]]) => map2(a, b)(_ :: _))
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }


  // exercise 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {//as match {
    //as.foldRight[Par[List[A]]](lazyUnit(Nil))((a: A, b: Par[List[A]]) => if(f(a)) List(a) else List())
    /*
    case Nil => lazyUnit(List())
    case x::xs => parMap()
      if f(x) fork(x+:parFilter(xs)(f))
      else parFilter(xs)(f)
      */
    ???
  }


  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)


  // exercise 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    run(es)(choices(run(es)(n).get))
  }

  def choiceViaN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))
  }

  // exercise 7.12
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    chooser(key)(choices)


  // exercise 7.13
  def chooser[A, B](cond: Par[A])(choices: A => Par[B]) : Par[B] =
    es => {
      run(es)(choices(run(es)(cond).get))
    }

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(i => choices(i))

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(b => if (b) t else f)

  //exercise 7.14
  def join[A](a: Par[Par[A]]): Par[A] =
    es => {
      val test: Par[A] = run(es)(a).get
      run(es)(test)
    }

  def flatMapViaJoin[A, B](cond: Par[A])(choices: A => Par[B]) : Par[B] = {
    join(map(cond)(choices))
  }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = {
    chooser(a)(x => x)
  }


}