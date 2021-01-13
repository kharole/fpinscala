package fpinscala.iomonad

import fpinscala.iomonad

object fp13 {

  //13.1
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    override def unit[A](a: => A): Free[F, A] = IO3.Return(a)

    override def flatMap[A, B](a: Free[F, A])(f: A => Free[F, B]): Free[F, B] = IO3.FlatMap(a, f)
  }

  //13.2
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case IO3.Return(a) => a
    case IO3.Suspend(s) => s()
    case IO3.FlatMap(s, f) => {
      s match {
        case IO3.Return(a) => runTrampoline(f(a))
        case IO3.Suspend(r) => runTrampoline(f(r()))
        case IO3.FlatMap(y, g) => runTrampoline(y flatMap (a => g(a) flatMap f))
      }
    }
  }
}
