package fpinscala.monads

import fpinscala.datastructures
import fpinscala.laziness.Stream
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

  //11.2
  class ZState[S] {
    type ZS[+A] = State[S, A]

    val stateMonad = new Monad[ZS] {
      override def unit[A](a: => A): ZS[A] = State.unit(a)

      override def flatMap[A, B](ma: ZS[A])(f: A => ZS[B]): ZS[B] = State(ma.run).flatMap(f)
    }
  }


}
