import scala.annotation.tailrec

object lab2 extends App {

    abstract class Token
    case object OpenBrace extends Token
    case object CloseBrace extends Token
    case object OpenBracket extends Token
    case object CloseBracket extends Token
    case object Colon extends Token
    case object Comma extends Token
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
            case '\\' :: '"' :: xs => parseString(acc + "\"", xs)
            case '\\' :: 'n' :: xs => parseString(acc + "\n", xs)
            case '"' :: xs => (acc, xs)
            case x :: xs => parseString(acc + x.toString, xs)
            case _ => throw new RuntimeException("Malformed String")
        }

        @tailrec
        def parseNumber(acc: String, rest: List[Char]): (String, List[Char]) = rest match {
            case x :: xs if List(')', ':', ',', ']').contains(x) => (acc, xs)
            case w :: xs if Character.isWhitespace(w) => (acc, xs)
            case Nil => (acc, Nil)
            case c :: xs => parseNumber(acc + c.toString, xs)
        }

        @tailrec
        def tokenize(acc: List[Token], rest: List[Char]): List[Token] = rest match {
            case w :: xs if Character.isWhitespace(w) => tokenize(acc, xs)
            case '{' :: xs => tokenize(acc :+ OpenBrace, xs)
            case '}' :: xs => tokenize(acc :+ CloseBrace, xs)
            case '[' :: xs => tokenize(acc :+ OpenBracket, xs)
            case ']' :: xs => tokenize(acc :+ CloseBracket, xs)
            case ':' :: xs => tokenize(acc :+ Colon, xs)
            case ',' :: xs => tokenize(acc :+ Comma, xs)
            case '"' :: xs =>
                val (string, rest) = parseString("", xs)
                tokenize(acc :+ StringToken(string), rest)
            case 'n' :: 'u' :: 'l' :: 'l' :: xs => tokenize(acc :+ NullToken, xs)
            case 't' :: 'r' :: 'u' :: 'e' :: xs => tokenize(acc :+ BooleanToken(true), xs)
            case 'f' :: 'a' :: 'l' :: 's' :: 'e' :: xs => tokenize(acc :+ BooleanToken(false), xs)
            case d :: xs =>
                val (number, rest) = parseNumber(d.toString, xs)
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
                    case CloseBrace :: xs => (JsonObject(list), xs)
                    case Comma :: xs => parseObject(list, xs)
                    case StringToken(s) :: Colon :: xs =>
                        val (json, rest) = parse(xs)
                        parseObject(list :+ (s, json), rest)
                    case _ => throw new RuntimeException("Incorrect object")
                }

            def parseArray(list: List[Json], rest: List[Token]): (Json, List[Token]) = rest match {
                case CloseBracket :: xs => (JsonArray(list), xs)
                case Comma :: xs => parseArray(list, xs)
                case value =>
                    val (json, rest) = parse(value)
                    parseArray(list :+ json, rest)
            }

            tokens match {
                case OpenBrace :: xs => parseObject(Nil, xs)
                case OpenBracket :: xs => parseArray(Nil, xs)
                case NullToken :: xs => (JsonNull, xs)
                case StringToken(str) :: xs => (JsonString(str), xs)
                case NumberToken(number) :: xs => (JsonNumber(number), xs)
                case BooleanToken(bool) :: xs => (JsonBoolean(bool), xs)
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
