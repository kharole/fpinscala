import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def simple: Parser[String] = ???

  def trimmed: Parser[String] = ???

  def string(s: String): Parser[String]

  implicit def char(c: Char): Parser[Char] = ???

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))

  import scala.util.matching.Regex

  implicit def regex(r: Regex): Parser[String]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B >: A](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def get1[A, B](p: Parser[(A, B)]): Parser[A] = p.map(_._1)

  //9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  //9.1
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    val pr: Parser[(A, B)] = product(p, p2)
    map(pr)(f.tupled)
  }

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))((a, as) => a :: as)

  //9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0)
      succeed(List())
    else
      map2(p, listOfN(n - 1, p))(_ :: _)


  //9.5
  def lazily[A](p: => Parser[A]): Parser[A] = p

  def many1_lazy[A](p: Parser[A]): Parser[List[A]] =
    map2(p, lazily(many(p)))((a, as) => a :: as)

  import fpinscala.testing._
  import fpinscala.testing.Prop.forAll

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def stringLaw(in: Gen[String]): Prop =
      forAll(in)(s => run(string(s))(s) == Right(s))

    //9.2
    def productDefLaw[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[(String, String)]): Prop =
      forAll(in)(sp => {
        val (s1, s2) = sp
        val yy = for {
          r1 <- run(p1)(s1)
          r2 <- run(p2)(s2)
        } yield ((r1, r2))

        val zz = run(product(p1, p2))(s1 + s2)
        yy == zz
      })

    def productAssocLaw[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]): Prop =
      forAll(in)(s => run(product(product(p1, p2), p3))(s) == run(product(p1, product(p2, p3)))(s))
  }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  //9.6
  def nAChars: Parser[Int] = {
    for {
      n <- regex("[0-9]*".r).map(_.toInt)
      _ <- listOfN(n, char('a'))
    } yield n
  }

  //9.7
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(a => p2.map(b => (a, b)))

  def map2_2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a, b)))

  //9.8
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(aa => succeed(f(aa)))

  //9.11
  def furthest[A](p: Parser[A]): Parser[A] = ???

  def latest[A](p: Parser[A]): Parser[A] = ???

  //9.10 - sucks
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def scope[A](e: String)(p: Parser[A]): Parser[A]

}

//9.9
trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  //trait Parser[JSON] {}
  def jsonParser[Err, Parser[+ _]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._
    import JSON._
    val spaces: Parser[String] = char(' ').many.slice

    val jpNull: Parser[JSON] = string("null").map(_ => JNull)
    val jpNumber: Parser[JSON] = regex("[0-9]*".r).map(d => JNumber(d.toInt))
    val jpString: Parser[JSON] = simple.map(s => JString(s))
    val jpBool: Parser[JSON] = regex("true|false".r).map(b => JBool(b.toBoolean))
    val jpArray: Parser[JSON] = (jsonParser(P) ** char(',')).map(_._1).many.map(js => JArray(js.toIndexedSeq))
    val jpObject: Parser[JSON] = ???

    jpNull | jpNumber | jpString | jpBool | jpArray | jpObject
  }

}

//9.18
trait ParseError2

case class ParseErrorSingle2(stack: List[(Location, String)] = List()) extends ParseError2
case class ParseErrorCombo() extends ParseError2

case class ParseError(stack: List[(Location, String)] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latest: Option[(Location, String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest map (_._1)

  //9.16
  override def toString =
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
          collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map { case (loc, msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") +
        context
    }

  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1).
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = l.line + "." + l.col
}

//9.17
case class Location(input: String, offset: Int = 0, isSliced: Boolean = false) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""

  def columnCaret = (" " * (col - 1)) + "^"
}

trait Result[+A] {
  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(e, c) => Failure(e, c || isCommitted)
    case _ => this
  }

  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a, m) => Success(a, n + m)
    case _ => this
  }

  def uncommit: Result[A] = this match {
    case Failure(e, true) => Failure(e, false)
    case _ => this
  }

  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e, c) => Failure(f(e), c)
    case _ => this
  }
}

case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

type MyParser2[+A] = Location => Result[A]

//9.13
object MyParser2 extends Parsers[String, MyParser2] {

  //9.15
  override def run[A](p: MyParser2[A])(input: String): Either[String, A] = p(Location(input)) match {
    case Success(a, _) => Right(a)
    case Failure(e, _) => Left(e.toString)
  }

  override def or[A](x: MyParser2[A], y: MyParser2[A]): MyParser2[A] =
    s => x(s) match {
      case Failure(_, false) => y(s)
      case r => r
    }

  override def string(s: String): MyParser2[String] =
    (loc: Location) =>
      if (loc.input.startsWith(s))
        Success(s, s.length)
      else
        Failure(ParseError(), false)

  override def succeed[A](a: A): MyParser2[A] =
    (loc: Location) =>
      Success(a, 0)

  override implicit def regex(r: Regex): MyParser2[String] =
    (loc: Location) =>
      r.findFirstIn(loc.input) match {
        case Some(value) => Success(value, value.length)
        case None => Failure(ParseError(), false)
      }

  override def slice[A](p: MyParser2[A]): MyParser2[String] =
    (loc: Location) =>
      Success(loc.input.substring(0, loc.offset), loc.offset)

  //9.14
  override def scope[A](e: String)(p: MyParser2[A]): MyParser2[A] = {
    s => p(s).mapError(_.push(s, e))
  }

  override def label[A](msg: String)(p: MyParser2[A]): MyParser2[A] =
    s => p(s).mapError(_.label(msg))

  override def flatMap[A, B](f: MyParser2[A])(g: A => MyParser2[B]): MyParser2[B] =
    s => f(s) match {
      case Success(a, n) => g(a)(s.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)

      case e@Failure(_, _) => e
    }


  override def attempt[A](p: MyParser2[A]): MyParser2[A] = {
    s => p(s).uncommit
  }
}


//9.12
class MyParser[+A](val f: String => List[(A, String, String)]) {
  def run(s: String): List[(A, String, String)] = f(s)
}

object MyParser extends Parsers[String, MyParser] {

  override def run[A](p: MyParser[A])(input: String): Either[String, A] = p.run(input) match {
    case x :: Nil => Right(x._1)
    case _ => Left("Error")
  }

  override def string(s: String): MyParser[String] = {
    val f = (x: String) => if (x.startsWith(s)) List((s, x.substring(s.length), s)) else Nil
    new MyParser[String](f)
  }

  override def regex(r: Regex): MyParser[String] = {
    val f = (x: String) => r.findFirstIn(x).map(m => (m, x.replaceFirst(m, ""), m)).toList
    new MyParser[String](f)
  }

  override def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = {
    val ff = (x: String) => p.f(x).flatMap(t => f(t._1).f(t._2).map(tt => (tt._1, tt._2, t._3 + tt._3)))
    new MyParser[B](ff)
  }

  override def slice[A](p: MyParser[A]): MyParser[String] = {
    val ff = (x: String) => p.f(x).map(t => (t._3, t._2, t._3))
    new MyParser[String](ff)
  }

  override def or[A](s1: MyParser[A], s2: MyParser[A]): MyParser[A] = {
    val ff = (x: String) => s1.f(x) ++ s2.f(x)
    new MyParser[A](ff)
  }

  //error handling
  override def label[A](msg: String)(p: MyParser[A]): MyParser[A] = ???

  override def attempt[A](p: MyParser[A]): MyParser[A] = ???

  override def scope[A](e: String)(p: MyParser[A]): MyParser[A] = ???
}
