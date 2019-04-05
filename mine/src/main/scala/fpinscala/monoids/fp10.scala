trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

//10.1
val intAddition: Monoid[Int] = ???
val intMultiplication: Monoid[Int] = ???
val booleanOr: Monoid[Int] = ???
val booleanAnd: Monoid[Int] = ???