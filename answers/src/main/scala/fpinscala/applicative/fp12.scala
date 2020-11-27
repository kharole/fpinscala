package fpinscala.applicative

import fpinscala.monads.{Functor, fp11}
import fpinscala.monads.fp11.{Monad, Reader}

import scala.collection.immutable

object fp12 {

  //noinspection DuplicatedCode
  //12.1
  trait Applicative[F[_]] extends Functor[F] {
    // primitive combinators
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def unit[A](a: => A): F[A]

    // derived combinators
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a, _) => f(a))

    def sequence[A](lma: List[F[A]]): F[List[A]] =
      lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

    // Using `sequence` and the `List.fill` function of the standard library:
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      sequence(List.fill(n)(ma))

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      map2(fa, fb)((a, b) => (a, b))

    //12.8
    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        override def unit[A](a: => A): (F[A], G[A]) =
          (Applicative.this.unit(a), G.unit(a))

        override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
          (Applicative.this.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }
  }

  //12.2
  trait Applicative2[F[_]] extends Functor[F] {
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fab, fa)((a2b, a) => a2b(a))

    def unit[A](a: => A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      map(apply(map(fa)(a => (b: B) => (a, b)))(fb))(f.tupled)

    //12.3
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(map(fa)(f.curried))(fb))(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

  }

}

object ApplicativeApp extends App {

  def goodStrings(elements: List[String], len: Int): List[String] = {
    if (len == 0) {
      List()
    } else {
      goodStrings(elements, len - 1)
    }
  }

  println("==========")
  println(goodStrings(List("a", "b", "c"), 2))
  //println((0 to 25).sum)

  //12.4 zip streams of incoming list to a stream of list of values of corresponding streams
  def sequence[A](a: List[Stream[A]]): Stream[List[A]] = ???

  //12.5
  object EitherM {
    def eitherMonad[E]: fp11.Monad[({type f[x] = Either[E, x]})#f] = new fp11.Monad[({type f[x] = Either[E, x]})#f] {

      def unit[A](a: => A): Either[E, A] =
        Right(a)

      def flatMap[A, B](e: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
        e match {
          case Left(value) => Left(value)
          case Right(value) => f(value)
        }

    }

  }


  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  //12.6
  object ValidationM {
    def validationApplicative[E]: fp12.Applicative[({type f[x] = Validation[E, x]})#f] =
      new fp12.Applicative[({type f[x] = Validation[E, x]})#f] {

        def unit[A](a: => A): Validation[E, A] =
          Success(a)

        override def map2[A, B, C](va: Validation[E, A], vb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
          (va, vb) match {
            case (Success(a), Success(b)) => Success(f(a, b))
            case (Success(_), Failure(h, t)) => Failure(h, t)
            case (Failure(h, t), Success(_)) => Failure(h, t)
            case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, (t1 ++ t2) :+ h2)
          }
      }

  }

  def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) =
    p match {
      case (a, (b, c)) => ((a, b), c)
    }

  //12.7
  /*
        monadic:
        associative
        x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

        identity:
        flatMap(x)(unit) == x
        flatMap(unit(y))(f) == f(y)

        def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a,b)))

        applicative:
        //left right identity
        map(v)(id) == v map(map(v)(g))(f) == map(v)(f compose g)
        map2(unit(()), fa)((_,a) => a) == fa
        flatMap(unit(())(u => map(fa)((_,a) => a))
        (u => map(fa)((_,a) => a))(())
        fa
        map2(fa, unit(()))((a,_) => a) == fa

        op(a, op(b, c)) == op(op(a, b), c)
        compose(f, op(g, h)) == compose(compose(f, g), h)

        //associativity
        product(product(fa,fb),fc) == map(product(fa, product(fb,fc)))(assoc)

        //associativity
        map2(a,b)(productF(f,g)) == product(map(a)(f), map(b)(g))
   */

}
