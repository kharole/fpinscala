import src.main.scala.fpinscala.monoids.{Gen, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

//10.1
val intAddition: Monoid[Int] = new Monoid[Int] {
  override def op(a1: Int, a2: Int) = a1 + a2

  override def zero = 0
}

val intMultiplication: Monoid[Int] = new Monoid[Int] {
  override def op(a1: Int, a2: Int) = a1 * a2

  override def zero = 1
}

val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
  override def op(a1: Boolean, a2: Boolean) = a1 || a2

  override def zero = false
}

val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
  override def op(a1: Boolean, a2: Boolean) = a1 && a2

  override def zero = true
}

//10.2
def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
  override def op(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
    case (None, None) => None
    case (Some(aa1), None) => Some(aa1)
    case (None, Some(aa2)) => Some(aa2)
    case (Some(aa1), Some(_)) => Some(aa1)
  }

  override def zero = None
}

//10.3
def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  override def op(a1: A => A, a2: A => A): A => A = a1.andThen(a2)

  override def zero: A => A = identity
}

//10.4
//def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???