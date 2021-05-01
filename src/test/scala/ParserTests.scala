
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
