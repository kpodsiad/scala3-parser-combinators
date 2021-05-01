package me.kpodsiad.parserCombinators
import Parser.ParserResult

case class Parser[T1](parseFn: String => ParserResult[T1]):
  import Parser._
  private val self = this

  def parse(str: String): ParserResult[T1] = this.parseFn(str)

  def map[T2](f: T1 => T2): Parser[T2] = this.flatMap(value => Parser.pure(f(value)) )

  def flatMap[T2](f: T1 => Parser[T2]): Parser[T2] = Parser { str =>
    this.parseFn(str) match {
      case Success(parsed, remaining) => f(parsed).parse(remaining)
      case Failure(msg) => Failure(msg)
    }
  }

  def andThen[T2](next: Parser[T2]): Parser[(T1, T2)] =
    for
      parsed1 <- self
      parsed2 <- next
    yield (parsed1, parsed2)

  def orElse[T2](other: Parser[T2]): Parser[T1 | T2] = Parser { str =>
    this.parse(str) match {
      case Success(parsed, remaining) => Success(parsed, remaining)
      case Failure(msg) =>
        other.parse(str) match {
          case Success(parsed, remaining) => Success(parsed, remaining)
          case Failure(msg) => Failure(msg)
        }
    }
  }

  def discardLeft[T2](right: Parser[T2]): Parser[T2] = this.andThen(right).map { case(t1, t2) => t2 }
  def discardRight[T2](right: Parser[T2]): Parser[T1] = this.andThen(right).map { case(t1, t2) => t1 }

object Parser:
  sealed trait ParserResult[A] {
    def isFailure = this match {
      case _: Success[A] => false
      case _: Failure[A] => true
    }
  }
  private[Parser] final case class Success[T](parsed: T, remaining: String) extends ParserResult[T]
  private[Parser] final case class Failure[T](msg: String) extends ParserResult[T]

  object ParserResult:
    def success[A](parsed: A, remaining: String) = Success(parsed, remaining)

    def failure[A](msg: String) = Failure(msg)

  def pure[A](a: A): Parser[A] = Parser(str => Success(a, str))
  def failure[A](msg: String): Parser[A] = Parser(str => Failure(msg))
  def ap[T1, T2](fP: Parser[T1 => T2])(xP: Parser[T1]): Parser[T2] =
    for
      f <- fP
      x <- xP
    yield f(x)

  def lift2[T1, T2, T3](f: T1 => T2 => T3)(first: Parser[T1])(second: Parser[T2]): Parser[T3] =
    for
      f <- pure(f)
      x <- first
      y <- second
    yield f(x)(y)

  def sequence[T](parsers: List[Parser[T]]): Parser[List[T]] =
    def cons(head: T)(tail: List[T]) = head :: tail
    def consP = lift2(cons)
    parsers match {
      case head :: tail => consP(head)(sequence(tail))
      case Nil => pure(Nil)
    }

  def choice[T](parsers: Seq[Parser[T]]): Parser[T] = parsers.reduce(_ orElse _)

  def anyOf(chars: Seq[Char]): Parser[Char] = choice(chars.map(charParser))

  private def parseZeroOrMore[T](parser: Parser[T])(str: String): Tuple2[List[T], String] =
    parser.parse(str) match {
      case Failure(msg) => (Nil, str)
      case Success(head, remaining1) =>
        val (tail, remaining2) = parseZeroOrMore(parser)(remaining1)
        (head :: tail, remaining2)
    }

  def many[T](parser: Parser[T]): Parser[List[T]] = Parser { str =>
    val (parsed, rest) = parseZeroOrMore(parser)(str)
    Success(parsed, rest)
  }

  def many1[T](parser: Parser[T]): Parser[List[T]] = Parser { str =>
    parser.parse(str) match {
      case Failure(msg) => Failure(msg)
      case Success(head, remaining) =>
        val (tail, rest) = parseZeroOrMore(parser)(remaining)
        Success(head :: tail, rest)
    }
  }

  def opt[T](parser: Parser[T]): Parser[Option[T]] = parser.map(Some(_)) orElse pure(None)

  def between[T1, T2, T3](p1: Parser[T1])(p2: Parser[T2])(p3: Parser[T3]): Parser[T2] =
    p1.discardLeft(p2).discardRight(p3)

  def separatedBy1[T1, T2](parser: Parser[T1])(separator: Parser[T2]): Parser[List[T1]] =
    parser.andThen(many(separator.discardLeft(parser))).map { case (head, tail) => head :: tail }

  def separatedBy[T1, T2](parser: Parser[T1])(separator: Parser[T2]): Parser[List[T1]] =
    separatedBy1(parser)(separator) orElse pure(Nil)

  def charParser(charToMatch: Char): Parser[Char] = Parser { str =>
    if str.length == 0 then Failure("No more input")
    else if str(0) == charToMatch then Success(charToMatch, str.substring(1))
    else Failure(s"Expecting $charToMatch, found ${str(0)}")
  }

  def stringParser(str: String): Parser[String] = sequence(str.toList.map(charParser)).map(_.mkString(""))

  val intParser: Parser[Int] =
    val digit = anyOf('0' to '9')
    opt(charParser('-')).andThen(many1(digit).map(_.mkString("").toInt)).map { case (sign, int) =>
      sign.fold(int)(_ => -int)
    }

