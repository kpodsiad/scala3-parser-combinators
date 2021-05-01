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

  def lift2[T1, T2, T3](f: T1 => T2 => T3)(first: Parser[T1])(second: Parser[T2]): Parser[T3] = {
    for
      f <- Parser.pure(f)
      x <- first
      y <- second
    yield f(x)(y)
  }

  def sequence[T](parsers: List[Parser[T]]): Parser[List[T]] = {
    def cons(head: T)(tail: List[T]) = head :: tail
    def consP = lift2(cons)
    parsers match {
      case head :: tail => consP(head)(sequence(tail))
      case Nil => Parser.pure(Nil)
    }
  }

  def choice[T](parsers: Seq[Parser[T]]): Parser[T] = parsers.reduce(_ orElse _)

  def anyOf(chars: Seq[Char]): Parser[Char] = choice(chars.map(charParser))

  def charParser(charToMatch: Char): Parser[Char] = Parser { str =>
    if str.length == 0 then Failure("No more input")
    else if str(0) == charToMatch then Success(charToMatch, str.substring(1))
    else Failure(s"Expecting $charToMatch, found ${str(0)}")
  }

  def stringParser(str: String): Parser[String] = sequence(str.toList.map(charParser)).map(_.mkString(""))