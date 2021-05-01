
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
