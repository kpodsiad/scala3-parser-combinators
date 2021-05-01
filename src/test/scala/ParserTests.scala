
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
    val digitParser = anyOf('0' to '9')

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
    val digitParser = anyOf('0' to '9')

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
