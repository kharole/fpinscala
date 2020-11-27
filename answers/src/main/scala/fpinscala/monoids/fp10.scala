import fp10.{Monoid, OrderedSegment, Part, Segment, Stub, intAddition, monoidLaws}
import fpinscala.parallelism.Nonblocking.Par
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

  val stringConcatenation: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String) = a1 + a2

    override def zero = ""
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

  //10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.size match {
    case 0 =>
      m.zero
    case 1 =>
      f(v.head)
    case _ =>
      val (v1, v2) = v.splitAt(v.size / 2)
      val b1: B = foldMapV(v1, m)(f)
      val b2: B = foldMapV(v2, m)(f)
      m.op(b1, b2)
  }

  //10.8

  import fpinscala.parallelism.Nonblocking._
  import fpinscala.parallelism.Nonblocking.Par.toParOps

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(pa1: Par[A], pa2: Par[A]): Par[A] = Par.map2(pa1, pa2)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(as)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  //10.9
  sealed trait Segment

  case object ZeroSegment extends Segment

  case class OrderedSegment(left: Int, right: Int) extends Segment

  case object UnorderedSegment extends Segment

  val segmentMerge: Monoid[Segment] = new Monoid[Segment] {
    override def op(s1: Segment, s2: Segment): Segment = (s1, s2) match {
      case (_, UnorderedSegment) => UnorderedSegment
      case (UnorderedSegment, _) => UnorderedSegment
      case (ZeroSegment, s) => s
      case (s, ZeroSegment) => s
      case (OrderedSegment(l1, r1), OrderedSegment(l2, r2)) if r1 <= l2 => OrderedSegment(l1, r2)
      case (OrderedSegment(_, r1), OrderedSegment(l2, _)) if r1 > l2 => UnorderedSegment
      case _ => throw new IllegalStateException
    }

    override def zero: Segment = ZeroSegment
  }

  object WC {
    def apply(chars: String): WC = {
      val a = chars.split("\\s+")
      a.size match {
        case 1 | 2 => Stub(chars)
        case _ => Part(a(0), a.size - 2, a(a.size - 1))
      }
    }
  }

  sealed trait WC {
    def count: Int = this match {
      case Stub(chars) =>
        chars.split("\\s+").size
      case Part(lStub, words, rStub) =>
        val l = if (lStub.isEmpty) 0 else 1
        val r = if (rStub.isEmpty) 0 else 1
        l + words + r
    }
  }

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC {
    def simplifyRight: Part = {
      val a = rStub.split("\\s+")
      a.size match {
        case 1 => this
        case 2 => Part(lStub, words + 1, a(1))
        case _ => throw new IllegalStateException()
      }
    }

    def simplifyLeft: Part = {
      val a = lStub.split("\\s+")
      a.size match {
        case 1 => this
        case 2 => Part(a(0), words + 1, rStub)
        case _ => throw new IllegalStateException()
      }
    }
  }

  //10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match {
      case (Stub(cs1), Stub(cs2)) => WC(cs1 + cs2)
      case (Part(ls1, w1, rs1), Stub(c2)) => Part(ls1, w1, rs1 + c2).simplifyRight
      case (Stub(c1), Part(ls2, w2, rs2)) => Part(c1 + ls2, w2, rs2).simplifyLeft
      case (Part(ls1, w1, ""), Part("", w2, rs2)) => Part(ls1, w1 + w2, rs2)
      case (Part(ls1, w1, _), Part(_, w2, rs2)) => Part(ls1, w1 + w2 + 1, rs2)
    }

    override def zero: WC = Stub("")

  }

  //10.11
  def wordCount(s: String): Int = {
    val wcs = rwc(s)
    val wc = foldMapV(wcs, wcMonoid)(identity)
    wc.count
  }

  def rwc(s: String): Vector[WC] = {
    if (s.size > 3) {
      val (s1, s2) = s.splitAt(s.size / 2)
      rwc(s1) ++ rwc(s2)
    } else {
      Vector(WC(s))
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  }

  //10.12

  val foldableList = new Foldable[List] {
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match {
      case head :: tail => f(foldLeft(tail)(z)(f), head)
      case Nil => z
    }

    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as match {
      case head :: tail => f(head, foldRight(tail)(z)(f))
      case Nil => z
    }

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
  }

  val indexedSeqList = new Foldable[IndexedSeq] {
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = {
      def foldLeftFrom[A, B](as: IndexedSeq[A], idx: Int)(z: B)(f: (B, A) => B): B =
        if (idx < as.length)
          foldLeftFrom(as, idx + 1)(f(z, as(idx)))(f)
        else
          z

      foldLeftFrom(as, 0)(z)(f)
    }

    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = {
      def foldRightFrom[A, B](as: IndexedSeq[A], idx: Int)(z: B)(f: (A, B) => B): B =
        if (idx > 0)
          foldRightFrom(as, idx - 1)(f(as(idx), z))(f)
        else
          z

      foldRightFrom(as, as.length)(z)(f)
    }

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
  }

  val foldableStream = new Foldable[Stream] {
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as match {
      case cons: Stream.Cons[A] => foldLeft(cons.tail)(f(z, cons.head))(f)
      case Stream.Empty => z
    }

    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as match {
      case cons: Stream.Cons[A] => f(cons.head, foldRight(cons.tail)(z)(f))
      case Stream.Empty => z
    }

    override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  //10.13
  val foldableTree = new Foldable[Tree] {
    override def foldLeft[A, B](t: Tree[A])(z: B)(f: (B, A) => B): B = t match {
      case Leaf(value) => f(z, value)
      case Branch(left, right) => {
        val lz = foldLeft(left)(z)(f)
        foldLeft(right)(lz)(f)
      }
    }

    override def foldRight[A, B](t: Tree[A])(z: B)(f: (A, B) => B): B = t match {
      case Leaf(value) => f(value, z)
      case Branch(left, right) => {
        val rz = foldRight(right)(z)(f)
        foldRight(left)(rz)(f)
      }
    }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
  }

  object MonoidsApp extends App {
    val ml = monoidLaws(intAddition, Gen.choose(-1000, 1000))
    Prop.run(ml)

    println(fp10.foldRight(List(1, 2, 3), "")((i, s) => s + i))

    println(fp10.foldMapV(IndexedSeq("lorem", "ipsum", "dolor", "sit"), fp10.stringConcatenation)(identity))

    val pool = java.util.concurrent.Executors.newFixedThreadPool(4)
    val zz: Par[String] = fp10.parFoldMap(IndexedSeq("lorem", "ipsum", "dolor", "sit"), fp10.stringConcatenation)(identity)
    println(Par.run(pool)(zz))

    //fp10.foldMapV(IndexedSeq(), fp10.segmentMerge)()

    println(fp10.foldMapV(IndexedSeq(1, 2, 5, 100), fp10.segmentMerge)(i => OrderedSegment(i, i)))
    println(fp10.foldMapV(IndexedSeq(2, 1, 5, 100), fp10.segmentMerge)(i => OrderedSegment(i, i)))
    println(fp10.foldMapV(IndexedSeq(), fp10.segmentMerge)(i => OrderedSegment(i, i)))

    pool.shutdown();

    println(fp10.wcMonoid.op(Stub("lor"), Stub("em")))
    println(fp10.wcMonoid.op(Stub("lor"), Stub("em ip")))
    println(fp10.wcMonoid.op(Stub("lor"), Stub("em ipsum do")))
    println(fp10.wcMonoid.op(Part("lorem", 1, ""), Part("dolor", 1, "amet")))
    println(fp10.wcMonoid.op(Part("lorem", 1, "do"), Stub("lor sit")))
    println(fp10.wcMonoid.op(Stub("lor"), Part("em", 1, "do")))
    println(fp10.wcMonoid.op(Stub("lorem ip"), Part("sum", 1, "si")))

    println(fp10.wordCount("lorem ipsum dolor sit amet "))

  }
