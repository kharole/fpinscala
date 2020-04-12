package fpinscala.applicative

import fpinscala.monads.Functor

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

    /*    {
          val fab: F[(A, B)] = apply(unit((a: A) => {
            val ab: (A, B) = apply(unit((b: B) => (a, b)))(fb)
            ab
          }))(fa)

          map(fab)(f.tupled)
        }*/

  }

}

object ApplicativeApp extends App {
}
