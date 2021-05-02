package me.kpodsiad.yamlParser

import me.kpodsiad.parserCombinators.Parser
import me.kpodsiad.parserCombinators.Parser._

import scala.deriving._
import scala.compiletime._

/**
 * poc for generic parser derivation
 * It's not
 */
trait Yamlify[T] {
  def parser: Parser[T]
}

object Yamlify:
  val spaces = many(charParser(' '))
  val colonWithSpace = between(spaces)(charParser(':'))(spaces)
  val newLine = charParser('\n')

  inline given string: Yamlify[String] = new Yamlify[String] {
    override def parser: Parser[String] = many(satisfy(_ != '\n')("any char")).map(_.mkString(""))
  }

  inline given int: Yamlify[Int] = new Yamlify[Int] {
    override def parser: Parser[Int] = intParser
  }

  inline def derived[T](using m: Mirror.Of[T]): Yamlify[T] = {
    val elemInstances = summonAll[m.MirroredElemTypes]
    val elemLabels = getElemLabels[m.MirroredElemLabels]
    inline m match {
      case p: Mirror.ProductOf[T] => productParser(p, elemInstances, elemLabels)
      case s: Mirror.SumOf[T]     => ???
    }
  }

  inline def summonAll[T <: Tuple]: List[Yamlify[_]] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[Yamlify[t]] :: summonAll[ts]
  }

  inline def getElemLabels[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (head *: tail) => constValue[head].toString :: getElemLabels[tail]
  }

  def productParser[T](p: Mirror.ProductOf[T], elems: List[Yamlify[_]], labels: List[String]): Yamlify[T] =
    new Yamlify[T] {
      def parser: Parser[T] = 
        sequence[AnyRef](elems.zip(labels).map { case (yamlify, label) =>
          stringParser(label).andThen(colonWithSpace).discardLeft(yamlify.parser).discardRight(newLine).asInstanceOf[Parser[AnyRef]]
        }).map(l => p.fromProduct(Tuple.fromArray(l.toArray)))
    }

