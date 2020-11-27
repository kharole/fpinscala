package fpinscala.monads

import fpinscala.datastructures
import fpinscala.laziness.Stream
import fpinscala.monads.fp11.Reader
import fpinscala.parallelism.Nonblocking.Par
import fpinscala.parsing.Sliceable
import fpinscala.parsing.SliceableTypes.Parser
import fpinscala.state.State

object fp11 {

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]

    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    def map[A, B](ma: F[A])(f: A => B): F[B] =
      flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    //11.3
    def sequence[A](lma: List[F[A]]): F[List[A]] = {
      val z = unit(List[A]())
      lma.foldLeft(z)((s, e) => map2(e, s)(_ :: _))
    }

    /*
          lma match {
          case head :: tail => flatMap(head)(h => map(sequence(tail))(t => h :: t))
          case Nil =>
        }
    */

    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(f))

    //11.4
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

    def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

    //11.6
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
      case head :: tail => flatMap(f(head)) { b =>
        if (b) {
          map(filterM(tail)(f))(t => head :: t)
        } else {
          filterM(tail)(f)
        }
      }
      case Nil => unit(Nil)
    }

    //11.7
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g(_))

    //11.8
    def flatMap2[A, B](ma: F[A])(f: A => F[B]): F[B] = {
      val u: Unit => F[A] = _ => ma
      val ff: Unit => F[B] = compose(u, f)
      ff(())
    }

    //11.9
    /*
      x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
      flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))

      compose(compose(f, g), h) == compose(f, compose(g, h))
      x => flatMap(compose(f, g)(x))(h)
      x => flatMap(flatMap(f(x))(g)(h)
      --
      x => flatMap(f(x))(compose(g, h))
      x => flatMap(f(x))(a => flatMap(g(a))(h))
     */

    //11.10
    /*
      compose(f, unit) == f
      x => flatMap(f(x))(unit)
      flatMap(x)(unit) == x

      compose(unit, f) == f
      y => flatMap(unit(y))(f)
      flatMap(unit(y))(f) == f(y)
     */

    //11.11
    /*
    Some(v).flatMap(unit) == Some(v)
    None.flatMap(unit) == None
     */

    //11.12
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

    //11.13
    def flatMap3[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

    //11.14
    /*
    flatMap(x)(unit) == x
    join(map(x)(unit)) == x

    flatMap(unit(y))(f) == f(y)
    join(map(unit(y))(f)) == f(y)
     */

    //11.15
    /*
    Regardless if u flatMap sequentially to f then to g or flatMap par computation to f flatMap g callback receives same result
    a followed by (b followed by c) is same parser as (a followed by b) followed by c
     */

    //11.16
    /*
    Gen flatMapped to unit produce same value as original Gen
    List flatMapped to unit first wrap each element to a single element list then unwrap in back
     */

    //12.11
    def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] =
        Monad.this.unit(G.unit(a))

      override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        ???
        //Monad.this.flatMap(na => G.flatMap(na)(a => ???))
    }
  }

  //11.1
  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  val parserMonad = new Monad[Parser] {
    override def unit[A](a: => A): Parser[A] = Sliceable.succeed(a)

    override def flatMap[A, B](ma: Parser[A])(f: A => Parser[B]): Parser[B] = Sliceable.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad = new Monad[datastructures.List] {
    override def unit[A](a: => A): datastructures.List[A] = datastructures.List(a)

    override def flatMap[A, B](ma: datastructures.List[A])(f: A => datastructures.List[B]): datastructures.List[B] = datastructures.List.flatMap(ma)(f)
  }

  val list2Monad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  //11.2
  class ZState[S] {
    type ZS[+A] = State[S, A]

    val stateMonad = new Monad[ZS] {
      override def unit[A](a: => A): ZS[A] = State.unit(a)

      override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)
    }
  }

  //11.17
  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))

    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f
  }

  def stateMonadLambda[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
  }

  val F = stateMonadLambda[Int]

  def getState[S]: State[S, S] = State(s => (s, s))

  def setState[S](s: S): State[S, Unit] = State(_ => ((), s))

  def zipWithIndex[A](as: List[A]): List[(Int, A)] = as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
    xs <- acc
    n <- getState
    _ <- setState(n + 1)
  } yield (n, a) :: xs).run(0)._1.reverse


  //11.18
  // sequence - transform list of states to a state of list
  //replicateM - repeat an element n times inside that state
  // map2 - join two states into one merging corresponding values with function

  //11.19
  //What laws do you expect to mutually hold for getState, setState, unit, and flatMap?

  val law1 = for {
    _ <- setState(42)
    n <- getState
  } yield n

  val law2: State[Unit, Unit] = for {
    n <- getState
    _ <- setState(n)
  } yield ()

  //11.20
  case class Reader[R, A](run: R => A)

  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
      def unit[A](a: => A): Reader[R, A] =
        Reader(_ => a)

      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader((r: R) => (st.run andThen f) (r).run(r))
    }
  }

}

object MonoadsApp extends App {
  //11.5 Cartesian product
  val is = List(1, 2)
  println(fp11.list2Monad.replicateM(2, is))

  val os = Some(true)
  println(fp11.optionMonad.replicateM(3, os))

  val irm = fp11.Reader.readerMonad[Int]
  val iir1: fp11.Reader[Int, Int] = fp11.Reader(_ + 1)
  val iir2: fp11.Reader[Int, Int] = fp11.Reader(_ + 2)
  val isr: fp11.Reader[Int, fp11.Reader[Int, String]] = fp11.Reader(i => fp11.Reader(ii => (ii + i).toString))

  //join - chaining incoming param to nested reader
  println(irm.join(isr).run(7))

  //replicateM - build list of n times reader runs
  println(irm.replicateM(7, iir1).run(5))

  //sequence - builds reader that given a param apply it to original readers and gathers the results to list
  println(irm.sequence(List(iir1, iir2)).run(5))


}
