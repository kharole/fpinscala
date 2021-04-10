package fpinscala.iomonad

import fpinscala.iomonad
import fpinscala.parallelism.Nonblocking.Par

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

  //13.3
  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case IO3.Suspend(x) => F.flatMap(x)((a: A) => run(IO3.Return(a)))
    case IO3.Return(a) => F.unit(a)
    case IO3.FlatMap(x, f) => x match {
      case IO3.Suspend(r) => F.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }

  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case IO3.FlatMap(IO3.FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case _ => a
  }

  sealed trait Console[A] {
    def toPar: Par[A]

    def toThunk: () => A
  }

  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)

    def toThunk = () => run

    def run: Option[String] =
      try Some(readLine())
      catch {
        case e: Exception => None
      }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))

    def toThunk = () => println(line)
  }

  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(
    implicit G: Monad[G]): G[A] =
    step(free) match {
      case IO3.Return(a) => G.unit(a)
      case IO3.Suspend(r) => t(r)
      case IO3.FlatMap(IO3.Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }

  //13.4
  def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = {
    type FreeG[A] = Free[G,A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G,A] = IO3.Suspend { fg(a) }
    }
    runFree(f)(t)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console,A]): A =
    runTrampoline { translate(a)(new (Console ~> Function0) {
      def apply[A](c: Console[A]) = c.toThunk
    })}



}
