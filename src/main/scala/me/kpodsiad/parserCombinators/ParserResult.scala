package me.kpodsiad.parserCombinators

import me.kpodsiad.parserCombinators.Parser.{Failure, ParserResult, Success, pure}

case class Parser[T1](parseFn: String => ParserResult[T1]):
  private val self = this

  def parse(str: String): ParserResult[T1] = this.parseFn(str)

  def map[T2](f: T1 => T2): Parser[T2] = Parser { str =>
    this.flatMap { value =>
      Parser.pure(f(value))
    }.parse(str)
  }

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

  def charParser(charToMatch: Char): Parser[Char] = Parser { str =>
    if str.length == 0 then Failure("No more input")
    else if str(0) == charToMatch then Success(charToMatch, str.substring(1))
    else Failure(s"Expecting $charToMatch, found ${str(0)}")
  }