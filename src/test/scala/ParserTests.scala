
import me.kpodsiad.parserCombinators.Parser
import org.junit.Assert._
import org.junit.Test
import me.kpodsiad.parserCombinators.Parser.ParserResult
import me.kpodsiad.parserCombinators.Parser._

class ParserTests:
  @Test def charParserTest(): Unit = {
    val aParser = charParser('A')
    assert(aParser.parse("AA") == ParserResult.success('A', "A"))
    assert(aParser.parse("BA").isFailure)
  }

  @Test def andThenTest(): Unit = {
    val aParser = charParser('A')
    val bParser = charParser('B')
    val abParser = aParser andThen bParser

    assert(abParser.parse("AB") == ParserResult.success(('A', 'B'), ""))
    assert(abParser.parse("AA").isFailure)
    assert(abParser.parse("BA").isFailure)
  }

  @Test def orElseTest(): Unit = {
    val aParser = charParser('A')
    val bParser = charParser('B')
    val aOrbParser: Parser[Char] = aParser orElse bParser

    assert(aOrbParser.parse("ABB") == ParserResult.success('A', "BB"))
    assert(aOrbParser.parse("BAA") == ParserResult.success('B', "AA"))
    assert(aOrbParser.parse("CAB").isFailure)
  }

  @Test def combineAndThenOrElseTest(): Unit = {
    val aParser = charParser('A')
    val bParser = charParser('B')
    val cParser = charParser('C')
    val aAndThenBorCParser: Parser[(Char, Char)] = aParser andThen (bParser orElse cParser)

    assert(aAndThenBorCParser.parse("ABC") == ParserResult.success(('A', 'B'), "C"))
    assert(aAndThenBorCParser.parse("ACB") == ParserResult.success(('A', 'C'), "B"))
    assert(aAndThenBorCParser.parse("AAA").isFailure)
  }

  @Test def tupleParser(): Unit = {
    val digitParser = digit

    val threeDigitAsStringParser = (digitParser andThen digitParser andThen digitParser).map {
      case ((first, second), third) => s"$first$second$third"
    }

    assert(threeDigitAsStringParser.parse("123") == ParserResult.success("123", ""))
    assert(threeDigitAsStringParser.parse("1234") == ParserResult.success("123", "4"))
    assert(threeDigitAsStringParser.parse("a123").isFailure)

    val threeDigitAsIntParser = threeDigitAsStringParser.map(_.toInt)
    assert(threeDigitAsIntParser.parse("123") == ParserResult.success(123, ""))
  }

  @Test def anyOfTest(): Unit = {
    val lowercaseParser = anyOf('a' to 'z')
    val digitParser = digit

    assert(lowercaseParser.parse("aGH") == ParserResult.success('a', "GH"))
    assert(lowercaseParser.parse("gHA") == ParserResult.success('g', "HA"))
    assert(lowercaseParser.parse("hAG") == ParserResult.success('h', "AG"))
    assert(lowercaseParser.parse("AGH").isFailure)

    assert(digitParser.parse("123") == ParserResult.success('1', "23"))
    assert(digitParser.parse("a23").isFailure)
  }

  @Test def sequenceTest(): Unit = {
    val parsers = List(charParser('A'), charParser('G'), charParser('H'))
    val aghParser = sequence(parsers)

    assert(aghParser.parse("AGH") == ParserResult.success(List('A', 'G', 'H'), ""))
    assert(aghParser.parse("AGH123") == ParserResult.success(List('A', 'G', 'H'), "123"))
    assert(aghParser.parse("UJ").isFailure)
  }

  @Test def stringParserTest(): Unit = {
    val aghParser = stringParser("AGH")

    assert(aghParser.parse("AGH") == ParserResult.success("AGH", ""))
    assert(aghParser.parse("AGH123") == ParserResult.success("AGH", "123"))
    assert(aghParser.parse("UJ").isFailure)
  }

  @Test def manyTest(): Unit = {
    val aManyParser = many(charParser('A'))
    assert(aManyParser.parse("AAAB") == ParserResult.success(List('A', 'A', 'A'), "B"))
    assert(aManyParser.parse("ABAB") == ParserResult.success(List('A'), "BAB"))

    val abMany = many(stringParser("AGH"))
    assert(abMany.parse("_AGH") == ParserResult.success(Nil, "_AGH"))
    assert(abMany.parse("AGHAGH") == ParserResult.success(List("AGH", "AGH"), ""))
    assert(abMany.parse("AGH_AGH") == ParserResult.success(List("AGH"), "_AGH"))

    val whitespaceParser = many(whitespace)
    assert(whitespaceParser.parse("") == ParserResult.success(Nil, ""))
    assert(whitespaceParser.parse(" ") == ParserResult.success(List(' '), ""))
    assert(whitespaceParser.parse(" \t \n") == ParserResult.success(List(' ', '\t', ' ', '\n'), ""))
  }

  @Test def many1Test(): Unit = {
    val abMany = many1(stringParser("AGH"))
    assert(abMany.parse("_AGH").isFailure)
    assert(abMany.parse("AGH_AGH") == ParserResult.success(List("AGH"), "_AGH"))
    assert(abMany.parse("AGHAGH") == ParserResult.success(List("AGH", "AGH"), ""))
  }

  @Test def optTest(): Unit = {
    val digitAndOptSemicolon = digit andThen opt(charParser(';'))
    assert(digitAndOptSemicolon.parse("1;") == ParserResult.success(('1', Some(';')), ""))
    assert(digitAndOptSemicolon.parse("1") == ParserResult.success(('1', None), ""))
  }

  @Test def intTest(): Unit = {
    assert(intParser.parse("1234") == ParserResult.success(1234, ""))
    assert(intParser.parse("1234A") == ParserResult.success(1234, "A"))
    assert(intParser.parse("-1234") == ParserResult.success(-1234, ""))
    assert(intParser.parse("-1234G") == ParserResult.success(-1234, "G"))
  }

  @Test def betweenTest(): Unit = {
    val quote = charParser('"')
    val quotedInt = between(quote)(intParser)(quote)

    assert(quotedInt.parse("\"1234\"") == ParserResult.success(1234, ""))
    assert(quotedInt.parse("\"1234\"A") == ParserResult.success(1234, "A"))
    assert(quotedInt.parse("\"-123\"4") == ParserResult.success(-123, "4"))
    assert(quotedInt.parse("\"-123\"4G") == ParserResult.success(-123, "4G"))
  }

  @Test def separatedTest(): Unit = {
    val sep = charParser(',')
    val start = charParser('[')
    val end = charParser(']')
    val list = between(start)(separatedBy(intParser)(sep))(end)

    assert(list.parse("[1,2,-997]") == ParserResult.success(List(1,2,-997), ""))
  }