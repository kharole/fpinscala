import fp10.{intAddition, monoidLaws}
import fpinscala.testing.Gen.listOfN
import fpinscala.testing.Prop.forAll
import fpinscala.testing.{Gen, Prop}

object fp10 {

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
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen)(a => m.op(a, m.zero) == a) &&
      forAll(listOfN(3, gen)) { case x :: y :: z :: Nil => m.op(m.op(x, y), z) == m.op(x, m.op(y, z)) }

  //10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldRight(m.zero)((a, s) => m.op(s, f(a)))

  //10.6
  def foldRight[A, B](as: List[A], z: B)(ff: (A, B) => B): B = {
    val f = (a: A) => (b: B) => ff(a, b)
    val mf: B => B = foldMap(as, endoMonoid[B])(f)
    mf(z)
  }

}

object MonoidsApp extends App {
  val ml = monoidLaws(intAddition, Gen.choose(-1000, 1000))
  Prop.run(ml)

  println(fp10.foldRight(List(1, 2, 3), "")((i, s) => s + i))
}
