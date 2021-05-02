package me.kpodsiad.parserCombinators

import Parser.ParserResult

case class Parser[T1](parseFn: String => ParserResult[T1], label: String):
  import Parser._
  private val self = this

  def parse(str: String): ParserResult[T1] = this.parseFn(str)

  def map[T2](f: T1 => T2): Parser[T2] = this.flatMap(value => Parser.pure(f(value)))

  def flatMap[T2](f: T1 => Parser[T2]): Parser[T2] =
    Parser({ str =>
      this.parseFn(str) match {
        case Success(parsed, remaining) => f(parsed).parse(remaining)
        case Failure(error, label) => Failure(error, label)
      }
    }, UnknownLabel)

  def andThen[T2](next: Parser[T2]): Parser[(T1, T2)] = {
    for
      parsed1 <- self
      parsed2 <- next
    yield (parsed1, parsed2)
  }.copy(label = s"${this.label} andThen ${next.label}")

  def orElse[T2](other: Parser[T2]): Parser[T1 | T2] =
    val label = s"${this.label} orElse ${other.label}"
    Parser({ str =>
      this.parse(str) match {
        case Success(parsed, remaining) => Success(parsed, remaining)
        case Failure(error, label) =>
          other.parse(str) match {
            case Success(parsed, remaining) => Success(parsed, remaining)
            case Failure(error, label) => Failure(error, label)
          }
      }
    }, label)


  def discardLeft[T2](right: Parser[T2]): Parser[T2] = this.andThen(right).map { case (t1, t2) => t2 }
  def discardRight[T2](right: Parser[T2]): Parser[T1] = this.andThen(right).map { case (t1, t2) => t1 }

object Parser:
  sealed trait ParserResult[A] {
    def isFailure: Boolean = this match {
      case _: Success[A] => false
      case _: Failure[A] => true
    }

    def showResult: String = this match {
      case Success(parsed, remaining) => parsed.toString
      case Failure(error, label) => s"Error parsing $label\n$error"
    }
  }
  private[Parser] final case class Success[T](parsed: T, remaining: String) extends ParserResult[T]
  private[Parser] final case class Failure[T](error: String, label: String) extends ParserResult[T]

  object ParserResult:
    def success[A](parsed: A, remaining: String): Success[A] = Success(parsed, remaining)
    def failure[A](error: String, label: String): Failure[A] = Failure(error, label)

  private final val UnknownLabel = "unknown"
  private final val PureLabel = "pure"

  def pure[A](a: A, label: String = PureLabel): Parser[A] = Parser(str => Success(a, str), label)
  def failed[A](msg: String, error: String): Parser[A] = Parser(str => Failure(msg, error), "failed")

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

  def anyOf(chars: Seq[Char]): Parser[Char] = choice(chars.map(charParser)).copy(label = s"anyOf ${chars}")

  private def parseZeroOrMore[T](parser: Parser[T])(str: String): Tuple2[List[T], String] =
    parser.parse(str) match {
      case _: Failure[T] => (Nil, str)
      case Success(head, remaining1) =>
        val (tail, remaining2) = parseZeroOrMore(parser)(remaining1)
        (head :: tail, remaining2)
    }

  def many[T](parser: Parser[T]): Parser[List[T]] = Parser({ str =>
    val (parsed, rest) = parseZeroOrMore(parser)(str)
    Success(parsed, rest)
  }, s"zero or more ${parser.label}")

  def many1[T](parser: Parser[T]): Parser[List[T]] = Parser({ str =>
    parser.parse(str) match {
      case Failure(error, label) => Failure(error, label)
      case Success(head, remaining) =>
        val (tail, rest) = parseZeroOrMore(parser)(remaining)
        Success(head :: tail, rest)
    }
  }, s"at least one ${parser.label}")

  def opt[T](parser: Parser[T]): Parser[Option[T]] = parser.map(Some(_)).orElse(pure(None)).copy(label = s"optional ${parser.label}")

  def between[T1, T2, T3](p1: Parser[T1])(p2: Parser[T2])(p3: Parser[T3]): Parser[T2] =
    p1.discardLeft(p2).discardRight(p3)

  def separatedBy1[T1, T2](parser: Parser[T1])(separator: Parser[T2]): Parser[List[T1]] =
    parser.andThen(many(separator.discardLeft(parser))).map { case (head, tail) => head :: tail }

  def separatedBy[T1, T2](parser: Parser[T1])(separator: Parser[T2]): Parser[List[T1]] =
    separatedBy1(parser)(separator) orElse pure(Nil)

  def satisfy(predicate: Char => Boolean)(label: String): Parser[Char] =
    Parser({ str =>
      if str.length == 0 then Failure("No more input", "")
      else if predicate(str(0)) then Success(str(0), str.substring(1))
      else Failure(label, s"Unexpected ${str(0)}")
    }, label)

  def charParser(charToMatch: Char): Parser[Char] = satisfy(x => x == charToMatch)(s"$charToMatch")

  val digit: Parser[Char] = satisfy(_.isDigit)("digit")
  val whitespace: Parser[Char] = satisfy(_.isWhitespace)("whitespace")

  def stringParser(str: String): Parser[String] =
    sequence(str.toList.map(charParser)).map(_.mkString(""))
      .copy(label = str)

  val intParser: Parser[Int] =
    opt(charParser('-')).andThen(many1(digit).map(_.mkString("").toInt)).map { case (sign, int) =>
      sign.fold(int)(_ => -int)
    }

