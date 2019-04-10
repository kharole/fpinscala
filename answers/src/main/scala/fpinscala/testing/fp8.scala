import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par

object fp8 {
  import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

  import RNG._
  import fpinscala.parallelism.Actor
  import fpinscala.testing.Prop.Passed

  import scala.collection.immutable
  //8.1
  /*
  sum: List[Int] => Int

  import  fpinscala.testing.Gen

  val intList = Gen.listOf(Gen.choose(0,100))
  val prop = forAll(intList)(ns => sum(ns.reverse) == sum(ns))

  val intList = List.fill(100)(Gen.choose(0,100))
  val prop = forAll(intList)(ns => sum(ns) == 100*ns.head)

  val prop = forAll(intList)(ns => sum(ns) == ns.head + sum(ns.tail))

  */

  //8.2
  /*
  max: List[Int] => Int //listOf1

  val prop = forAll(intList)(ns => ns.forall(_ <= max(ns)))

  */

  //8.3

  trait Prop2 {
    def check: Boolean

    def &&(p: Prop2): Prop2 = new Prop2 {
      def check: Boolean = this.check && p.check
    }
  }

  trait RNG {
    def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
  }

  object RNG {

    // NB - this was called SimpleRNG in the book text

    case class Simple(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
        val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
        val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
        (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      }
    }

    // We need to be quite careful not to skew the generator.
    // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
    // it suffices to increment the negative numbers by 1 and make them positive.
    // This maps Int.MinValue to Int.MaxValue and -1 to 0.
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 1) else i, r)
    }

    // We generate an integer >= 0 and divide it by one higher than the
    // maximum. This is just one possible solution.
    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + 1), r)
    }

    def boolean(rng: RNG): (Boolean, RNG) =
      rng.nextInt match {
        case (i, rng2) => (i % 2 == 0, rng2)
      }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r1) = rng.nextInt
      val (d, r2) = double(r1)
      ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), r) = intDouble(rng)
      ((d, i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d3, r3) = double(r2)
      ((d1, d2, d3), r3)
    }

    // There is something terribly repetitive about passing the RNG along
    // every time. What could we do to eliminate some of this duplication
    // of effort?

    // A simple recursive solution
    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
      if (count == 0)
        (List(), rng)
      else {
        val (x, r1) = rng.nextInt
        val (xs, r2) = ints(count - 1)(r1)
        (x :: xs, r2)
      }

    // A tail-recursive solution
    def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
      def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
        if (count == 0)
          (xs, r)
        else {
          val (x, r2) = r.nextInt
          go(count - 1, r2, x :: xs)
        }

      go(count, rng, List())
    }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    val _double: Rand[Double] =
      map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

    // This implementation of map2 passes the initial RNG to the first argument
    // and the resulting RNG to the second argument. It's not necessarily wrong
    // to do this the other way around, since the results are random anyway.
    // We could even pass the initial RNG to both `f` and `g`, but that might
    // have unexpected results. E.g. if both arguments are `RNG.int` then we would
    // always get two of the same `Int` in the result. When implementing functions
    // like this, it's important to consider how we would test them for
    // correctness.
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, r1) = ra(rng)
        val (b, r2) = rb(r1)
        (f(a, b), r2)
      }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] =
      both(int, double)

    val randDoubleInt: Rand[(Double, Int)] =
      both(double, int)

    // In `sequence`, the base case of the fold is a `unit` action that returns
    // the empty list. At each step in the fold, we accumulate in `acc`
    // and `f` is the current element in the list.
    // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
    // We map over that to prepend (cons) the element onto the accumulated list.
    //
    // We are using `foldRight`. If we used `foldLeft` then the values in the
    // resulting list would appear in reverse order. It would be arguably better
    // to use `foldLeft` followed by `reverse`. What do you think?
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

    // It's interesting that we never actually need to talk about the `RNG` value
    // in `sequence`. This is a strong hint that we could make this function
    // polymorphic in that type.

    def _ints(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(int))

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, r1) = f(rng)
        g(a)(r1) // We pass the new state along
      }

    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    }

    def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))

    def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }

  object State {
    type Rand[A] = State[RNG, A]

    def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

    // The idiomatic solution is expressed via foldRight
    def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
      sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

    // This implementation uses a loop internally and is the same recursion
    // pattern as a left fold. It is quite common with left folds to build
    // up a list in reverse order, then reverse it at the end.
    // (We could also use a collection.mutable.ListBuffer internally.)
    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
      def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
        actions match {
          case Nil => (acc.reverse, s)
          case h :: t => h.run(s) match {
            case (a, s2) => go(s2, t, a :: acc)
          }
        }

      State((s: S) => go(s, sas, List()))
    }

    // We can also write the loop using a left fold. This is tail recursive like the
    // previous solution, but it reverses the list _before_ folding it instead of after.
    // You might think that this is slower than the `foldRight` solution since it
    // walks over the list twice, but it's actually faster! The `foldRight` solution
    // technically has to also walk the list twice, since it has to unravel the call
    // stack, not being tail recursive. And the call stack will be as tall as the list
    // is long.
    def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] =
      l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }


  case class Gen[+A](sample: State[RNG, A]) {
    //8.6
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f.andThen(_.sample)))

    def map[B](f: A => B): Gen[B] = ???

    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(sz => Gen.listOfN(sz, this))

    //8.10
    def unsized: SGen[A] = SGen(_ => this)


    def **[B](g: Gen[B]): Gen[(A, B)] = ??? // (this map2 g)((_,_))
  }

  object Gen {

    //8.4
    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      val r = RNG.nonNegativeLessThan(stopExclusive - start)
      val rr = RNG.map(r)(_ + start)
      Gen(State(rr))
    }

    //8.5
    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      val s: Rand[A] = g.sample.run
      val list: List[Rand[A]] = List.fill(n)(s)
      Gen(State(sequence(list)))
    }

    //8.7
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen.boolean.flatMap(b => if (b) g1 else g2)

    //8.8
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
      Gen(State(RNG.double)).flatMap(d => if (d * ((g1._2 + g2._2)) < g1._2) g1._1 else g2._1)


    //8.19
    def fn[A, B](n: Int, a: Gen[A], b: Gen[B]): Gen[A => B] = {
      val gas = listOfN(n, a)
      val gbs = listOfN(n, b)
      for {
        as <- gas
        bs <- gbs
        d <- b
      } yield as.zip(bs).toMap.withDefault(_ => d)
    }
  }

  object Prop {
    type SuccessCount = Int
    type TestCases = Int
    type MaxSize = Int
    type FailedCase = String

    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      def isFalsified = false
    }

    case class Falsified(failure: FailedCase,
                         successes: SuccessCount) extends Result {
      def isFalsified = true
    }

    /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
    /*  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
        Stream.unfold(rng)(rng => Some(g.sample.run(rng)))*/

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = ???

    //8.15 - replace random stream with stream of all cases e.g. with implicit transformation

    /*    Prop {
        (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
          case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }.find(_.isFalsified).getOrElse(Passed)
      }*/

    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = ???


    // String interpolation syntax. A string starting with `s"` can refer to
    // a Scala value `v` as `$v` or `${v}` in the string.
    // This will be expanded to `v.toString` by the Scala compiler.
    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }


  case class Prop(run: (Prop.TestCases, RNG) => Prop.Result) {

    //8.9
    def &&(p: Prop): Prop = Prop { (n, rng) =>
      val r = run(n, rng)
      val pr = p.run(n, rng)
      (r, pr) match {
        case (Prop.Passed, Prop.Passed) => Prop.Passed
        case (f: Prop.Falsified, _) => f
        case (_, f: Prop.Falsified) => f
      }
    }

    def ||(p: Prop): Prop = Prop { (n, rng) =>
      val r = run(n, rng)
      val pr = p.run(n, rng)
      (r, pr) match {
        case (f: Prop.Falsified, pf: Prop.Falsified) => f
        case _ => Prop.Passed
      }
    }

  }

  object SGen {
    //8.12
    def listOfN[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))

    //8.13
    def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n + 1, g))

    private def fork[A](n: Int, g: Gen[Par[A]]): Gen[Par[A]] = if (n == 0) g else fork(n - 1, g.map(Par.fork(_)))

    //8.16
    def deepPar[A](g: Gen[A]): SGen[Par[A]] = SGen(n => fork(n, g.map(Par.unit(_))))

    //8.11
    def union[A](g1: SGen[A], g2: SGen[A]): SGen[A] = SGen(n => Gen.union(g1.forSize(n), g2.forSize(n)))

  }

  case class SGen[+A](forSize: Int => Gen[A]) {
    //8.11
    def flatMap[B](f: A => SGen[B]): SGen[B] =
      SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))


  }

  //8.14
  val intList = Gen.listOfN(100, Gen.choose(0, 100))
  val prop = Prop.forAll(intList) { ns =>
    val s = ns.sorted
    s.zip(s.tail).forall(p => p._1 > p._2)
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)

  val pint = Gen.choose(0, 10) map (Par.unit(_))

  val pint2: Gen[Par[Int]] = Gen.choose(-100, 100).listOfN(Gen.choose(0, 20)).map(l =>
    l.foldLeft(Par.unit(0))((p, i) =>
      Par.fork {
        Par.map2(p, Par.unit(i))(_ + _)
      }))

  val p4 =
    Prop.forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  //8.17
  val p5 =
    Prop.forAllPar(pint)(x => equal(Par.fork(x), x))

  //8.18
  val isEven = (i: Int) => i % 2 == 0
  val takeWhileProp =
    Prop.forAll(Gen.listOfN(100, Gen.choose(0, 100)))(ns => ns.takeWhile(isEven).forall(isEven))

  val takeWhileProp2 =
    Prop.forAll(Gen.listOfN(100, Gen.choose(0, 100))) { ns =>
      val dw = ns.dropWhile(isEven)
      val tw = ns.takeWhile(isEven)
      dw ::: tw == ns
    }

  //8.19
  def genStringFn[A](g: Gen[A]): Gen[String => A] = Gen {
    State { (rng: RNG) =>
      val (seed, rng2) = rng.nextInt
      val f = (s: String) => g.sample.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
      (f, rng2)
    }
  }

  trait Cogen[-A] {
    def sample(a: A, rng: RNG): RNG
  }

  class HashCogen[-A] extends Cogen[A] {
    def sample(a: A, rng: RNG): RNG = {
      val (seed, _) = rng.nextInt
      RNG.Simple(seed.toLong ^ a.hashCode.toLong)
    }
  }


  def fn[A, B](in: Cogen[A])(out: Gen[B]): Gen[A => B] = Gen {
    State { (rng: RNG) =>
      val f = (a: A) => out.sample.run(in.sample(a, rng))._1
      val (_, rng2) = rng.nextInt
      (f, rng2)
    }
  }


  //8.20
  val cg: Cogen[Int] = ???
  val gp: Gen[Int => Boolean] = fn(cg)(Gen.boolean)
  val gl = Gen.listOfN(100, Gen.choose(0, 100))

  val filterProp =
    Prop.forAll(gp ** gl) { case (p, l) => l.filter(p).forall(!p(_)) }
}