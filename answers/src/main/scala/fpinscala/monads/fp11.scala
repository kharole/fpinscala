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

}

object MonoadsApp extends App {
  //11.5 Cartesian product
  val is = List(1, 2)
  println(fp11.list2Monad.replicateM(2, is))

  val os = Some(true)
  println(fp11.optionMonad.replicateM(3, os))
}
