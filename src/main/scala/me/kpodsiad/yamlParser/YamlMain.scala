import me.kpodsiad.parserCombinators.Parser
import me.kpodsiad.parserCombinators.Parser._
import me.kpodsiad.yamlParser.Yamlify

case class MyCharacter(name: String, race: String, age: Int) derives Yamlify

object Main extends App {
  val yaml =
    """name: Frodo Baggins
      |race: Hobbit
      |age: 53
      |""".stripMargin

  val invalidYaml =
    """name: Frodo
      |Baggins
      |race: Hobbit
      |age: 53
      |""".stripMargin

  val characterParser = {
    val spaces = many(charParser(' '))
    val anyExceptNewline = many(satisfy(_ != '\n')("any char")).map(_.mkString(""))
    val colonWithSpace = between(spaces)(charParser(':'))(spaces)
    val newLine = charParser('\n')

    val name = stringParser("name")
      .andThen(colonWithSpace)
      .discardLeft(anyExceptNewline)

    val race = stringParser("race")
      .andThen(colonWithSpace)
      .discardLeft(anyExceptNewline)

    val age = stringParser("age")
      .andThen(colonWithSpace)
      .discardLeft(intParser)

    name.discardRight(newLine).andThen(race).discardRight(newLine).andThen(age).discardRight(newLine)
      .map { case ((name, race), age) =>
        MyCharacter(name, race, age)
      }
  }

  println(yaml)

  println(characterParser.parse(yaml))
  println(summon[Yamlify[MyCharacter]].parser.parse(yaml))

  println(characterParser.parse(invalidYaml))
  println(summon[Yamlify[MyCharacter]].parser.parse(invalidYaml))
}
