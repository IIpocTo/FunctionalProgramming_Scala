import scala.annotation.tailrec

object lab2 extends App {

    abstract class Token
    case object OpenCurlyBracket extends Token //{
    case object CloseCurlyBracket extends Token //}
    case object OpenSquareBracket extends Token //[
    case object CloseSquareBracket extends Token //]
    case object Colon extends Token //:
    case object Comma extends Token //,
    case object NullToken extends Token
    case class StringToken(string: String) extends Token
    case class NumberToken(double: Double) extends Token
    case class BooleanToken(boolean: Boolean) extends Token

    abstract class Json
    case class JsonObject(value: List[(String, Json)]) extends Json
    case class JsonArray(value : List[Json]) extends Json
    case class JsonString(value : String) extends Json
    case class JsonNumber(value : Double) extends Json
    case class JsonBoolean(value : Boolean) extends Json
    case object JsonNull extends Json

    def getTokens(source: String): List[Token] = {

        @tailrec
        def parseString(acc: String, rest: List[Char]): (String, List[Char]) = rest match {
            case '\\' :: '"' :: tail => parseString(acc + "\"", tail)
            case '\\' :: 'n' :: tail => parseString(acc + "\n", tail)
            case '"' :: tail => (acc, tail)
            case x :: tail => parseString(acc + x.toString, tail)
            case _ => throw new RuntimeException("Malformed String")
        }

        @tailrec
        def parseNumber(acc: String, rest: List[Char]): (String, List[Char]) = rest match {
            case x :: tail if List(')', ':', ',', ']').contains(x) => (acc, tail)
            case w :: tail if Character.isWhitespace(w) => (acc, tail)
            case Nil => (acc, Nil)
            case c :: tail => parseNumber(acc + c.toString, tail)
        }

        @tailrec
        def tokenize(acc: List[Token], rest: List[Char]): List[Token] = rest match {
            case w :: tail if Character.isWhitespace(w) => tokenize(acc, tail)
            case '{' :: tail => tokenize(acc :+ OpenCurlyBracket, tail)
            case '}' :: tail => tokenize(acc :+ CloseCurlyBracket, tail)
            case '[' :: tail => tokenize(acc :+ OpenSquareBracket, tail)
            case ']' :: tail => tokenize(acc :+ CloseSquareBracket, tail)
            case ':' :: tail => tokenize(acc :+ Colon, tail)
            case ',' :: tail => tokenize(acc :+ Comma, tail)
            case '"' :: tail =>
                val (string, rest) = parseString("", tail)
                tokenize(acc :+ StringToken(string), rest)
            case 'n' :: 'u' :: 'l' :: 'l' :: tail => tokenize(acc :+ NullToken, tail)
            case 't' :: 'r' :: 'u' :: 'e' :: tail => tokenize(acc :+ BooleanToken(true), tail)
            case 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tail => tokenize(acc :+ BooleanToken(false), tail)
            case double :: tail =>
                val (number, rest) = parseNumber(double.toString, tail)
                tokenize(acc :+ NumberToken(java.lang.Double.parseDouble(number)), rest)
            case Nil => acc
            case _ => throw new RuntimeException("Tokenizing error")
        }

        tokenize(Nil, source.toList)

    }

    private val tokens = getTokens(
        """{
                "id": "011A",
                "error": "Expected a ',' or '}' at 15 [character 16 line 1]",
                "price": "500\u00a3",
                "validate": false,
                "time": "03:53:25 AM",
                "value": 323e-1,
                "results":[
                    {
                        "text":"@twitterapi  http://tinyurl.com/ctrefg",
                        "to_user_id":396524,
                        "to_user":"TwitterAPI",
                        "from_user":"jkoum",
                        "metadata":
                        {
                            "result_type":"popular",
                            "recent_retweets": 109
                        },
                        "iso_language_code":"nl",
                        "source":"twitter<\\n>",
                        "profile_image_url":"http://s3.amazonaws.com/twitter_production/profile_images/118412707/2522215727_a5f07da155_b_normal.jpg",
                        "created_at":"Wed, 08 Apr 2009 19:22:10 +0000"
                    }
                ]
        }"""
    )

    println(tokens)

    def parseTokens(tokens: List[Token]): Json = {

        def parse(tokens: List[Token]): (Json, List[Token]) = {

            def parseObject(list: List[(String, Json)], rest: List[Token]): (Json, List[Token]) = rest match {
                    case CloseCurlyBracket :: tail => (JsonObject(list), tail)
                    case Comma :: tail => parseObject(list, tail)
                    case StringToken(s) :: Colon :: tail =>
                        val (json, rest) = parse(tail)
                        parseObject(list :+ (s, json), rest)
                    case _ => throw new RuntimeException("Incorrect object")
                }

            def parseArray(list: List[Json], rest: List[Token]): (Json, List[Token]) = rest match {
                case CloseSquareBracket :: tail => (JsonArray(list), tail)
                case Comma :: tail => parseArray(list, tail)
                case elem =>
                    val (json, rest) = parse(elem)
                    parseArray(list :+ json, rest)
            }

            tokens match {
                case OpenCurlyBracket :: tail => parseObject(Nil, tail)
                case OpenSquareBracket :: tail => parseArray(Nil, tail)
                case NullToken :: tail => (JsonNull, tail)
                case StringToken(str) :: tail => (JsonString(str), tail)
                case NumberToken(number) :: tail => (JsonNumber(number), tail)
                case BooleanToken(bool) :: tail => (JsonBoolean(bool), tail)
                case _ => throw new RuntimeException("Invalid token")
            }

        }

        parse(tokens) match {
            case (result, Nil) => result
            case _ => throw new RuntimeException("Wrong JSON structure")
        }

    }

    println(parseTokens(tokens))

}
