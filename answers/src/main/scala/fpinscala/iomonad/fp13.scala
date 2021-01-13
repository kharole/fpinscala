package fpinscala.iomonad

object fp13 {

  //13.1
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def unit[A](a: => A): Free[F, A] = IO3.Return(a)

    override def flatMap[A, B](a: Free[F, A])(f: A => Free[F, B]): Free[F, B] = IO3.FlatMap(a, f)
  }

}
