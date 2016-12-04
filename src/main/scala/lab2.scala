import scala.annotation.tailrec
import scala.util.Random

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
            case '\\' :: '\\' :: 'n' :: tail => parseString(acc + "\\n", tail)
            case '"' :: tail => (acc, tail)
            case x :: tail => parseString(acc + x.toString, tail)
            case _ => throw new RuntimeException("Malformed String")
        }

        @tailrec
        def parseNumber(acc: String, rest: List[Char]): (String, List[Char]) = rest match {
            case x :: tail if List(',', ']').contains(x) => (acc, tail)
            case w :: tail if Character.isWhitespace(w) => (acc, tail)
            case Nil => (acc, Nil)
            case c :: tail => parseNumber(acc + c.toString, tail)
            case _ => throw new RuntimeException("Malformed Number")
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
                        "source":"twitter<\"\n>",
                        "profile_image_url":"http://s3.amazonaws.com/twitter_production/profile_images/118412707/2522215727_a5f07da155_b_normal.jpg",
                        "created_at":"Wed, 08 Apr 2009 19:22:10 +0000"
                    }
                ]
        }"""
    )

    println(s"Tokens\n\n $tokens \n\n\n")

    val testJson: Json = parseTokens(tokens)
    println(s"JSON\n\n $testJson \n\n\n")

    def calculateHash(json: Json): Map[String, Int] = {

        val emptyHash: Map[String, Int] = Map(
            "JsonObject" -> 1,
            "JsonArray" -> 0,
            "JsonString" -> 0,
            "JsonNumber" -> 0,
            "JsonBoolean" -> 0,
            "JsonNull" -> 0
        )

        val innerList = json.asInstanceOf[JsonObject].value

        def updateValue(json: String, hash: Map[String, Int]): Map[String, Int] = {
            val oldValue = hash(json)
            val hashAfterDelete = hash - json
            val hashAfterUpdate = hashAfterDelete + (json -> (oldValue + 1))
            hashAfterUpdate
        }

        def getHash(list: List[(String, Json)], hash: Map[String, Int]): Map[String, Int]  = list match {
            case (_, JsonObject(obj)) :: tail => getHash(tail, getHash(obj, updateValue("JsonObject", hash)))
            case (_, JsonArray(arr)) :: tail => getHash(tail, getHashFromArray(arr, updateValue("JsonArray", hash)))
            case (_, JsonString(_)) :: tail => getHash(tail, updateValue("JsonString", hash))
            case (_, JsonBoolean(_)) :: tail => getHash(tail, updateValue("JsonBoolean", hash))
            case (_, JsonNumber(_)) :: tail => getHash(tail, updateValue("JsonNumber", hash))
            case (_, JsonNull) :: tail => getHash(tail, updateValue("JsonNull", hash))
            case Nil => hash
            case _ => throw new RuntimeException("Invalid json structure")
        }

        def getHashFromArray(list: List[Json], hash: Map[String, Int]): Map[String, Int] = list match {
            case JsonObject(obj) :: tail => getHashFromArray(tail, getHash(obj, updateValue("JsonObject", hash)))
            case JsonArray(arr) :: tail => getHashFromArray(tail, getHashFromArray(arr, updateValue("JsonArray", hash)))
            case JsonString(_) :: tail => getHashFromArray(tail, updateValue("JsonString", hash))
            case JsonBoolean(_) :: tail => getHashFromArray(tail, updateValue("JsonBoolean", hash))
            case JsonNumber(_) :: tail => getHashFromArray(tail, updateValue("JsonNumber", hash))
            case JsonNull :: tail => getHashFromArray(tail, updateValue("JsonNull", hash))
            case Nil => hash
            case _ => throw new RuntimeException("Invalid json structure")
        }

        getHash(innerList, emptyHash)

    }

    val jsonHash = calculateHash(testJson)
    println(s"Hash \n\n $jsonHash \n\n\n")

    def generateRandomJsonString(length: Int): JsonString = {
        val randomChar = Random.alphanumeric
        JsonString(randomChar take length mkString "")
    }

    def generateRandomString(length: Int): String = {
        val randomChar = Random.alphanumeric
        new String(randomChar take length mkString "")
    }

    def generateRandomJsonNumber(min: Int, max: Int, scale: Int): JsonNumber = {
        val randomDouble = (min + (max - min)) * Random.nextDouble()
        JsonNumber(BigDecimal(randomDouble).setScale(scale, BigDecimal.RoundingMode.CEILING).toDouble)
    }

    def generateRandomJsonArray(length: Int): JsonArray = {
        @tailrec
        def buildArray(jsonArray: List[Json]): JsonArray = {
            if (jsonArray.length == length) JsonArray(jsonArray)
            else {
                val randomValue = Random.nextInt(6)
                randomValue match {
                    case 0 => buildArray(jsonArray :+ JsonNull)
                    case 1 => buildArray(jsonArray :+ JsonBoolean(Random.nextBoolean))
                    case 2 => buildArray(jsonArray :+ generateRandomJsonString(Random.nextInt(20) + 1))
                    case 3 => buildArray(jsonArray :+ generateRandomJsonNumber(
                        -Random.nextInt(9999), Random.nextInt(9999), Random.nextInt(10)
                    ))
                    case 4 => buildArray(jsonArray :+ generateRandomJsonArray(Random.nextInt(5) + 1))
                    case 5 => buildArray(jsonArray :+ generateRandomJsonObject(Random.nextInt(5) + 1))
                    case _ => throw new RuntimeException("sth went wrong")
                }
            }
        }
        buildArray(Nil)
    }

    def generateRandomJsonObject(length: Int): JsonObject = {
        @tailrec
        def buildObject(jsonObject: List[(String, Json)]): JsonObject = {
            if (jsonObject.length == length) JsonObject(jsonObject)
            else {
                val randomValue = Random.nextInt(6)
                randomValue match {
                    case 0 => buildObject(jsonObject :+
                        (generateRandomString(Random.nextInt(20) + 1), JsonNull))
                    case 1 => buildObject(jsonObject :+
                        (generateRandomString(Random.nextInt(20) + 1), JsonBoolean(Random.nextBoolean)))
                    case 2 => buildObject(jsonObject :+
                        (generateRandomString(Random.nextInt(20) + 1), generateRandomJsonString(Random.nextInt(50) + 1)))
                    case 3 => buildObject(jsonObject :+
                        (generateRandomString(Random.nextInt(20) + 1), generateRandomJsonNumber(
                            -Random.nextInt(9999), Random.nextInt(9999), Random.nextInt(10)
                        )))
                    case 4 => buildObject(jsonObject :+
                        (generateRandomString(Random.nextInt(20) + 1), generateRandomJsonArray(Random.nextInt(5) + 1)))
                    case 5 => buildObject(jsonObject :+
                        (generateRandomString(Random.nextInt(20) + 1), generateRandomJsonObject(Random.nextInt(5) + 1)))
                    case _ => throw new RuntimeException("sth went wrong")
                }
            }
        }
        buildObject(Nil)
    }

    val randomJsonObject = generateRandomJsonObject(5)
    println(s"Random JSON\n\n" + randomJsonObject + "\n\n\n")


    def getOffset(n: Int): String = {
        if (n == 0) ""
        else (for (_ <- 1 to n) yield "\t").reduceLeft(_ ++ _)
    }

    def serialize(level: Int, json: Json): String = {
        val currentLevelOffset: String = getOffset(level)
        val nextLevelOffset: String = getOffset(level + 1)
        json match {
            case JsonNull => "null"
            case JsonBoolean(x) => x.toString
            case JsonNumber(x) => x.toString
            case JsonString(x) => "\"" + x.replaceAll("\\n", "\\\\n") + "\""
            case JsonArray(arr) =>
                "[\n" + arr
                    .map(value => nextLevelOffset + serialize(level + 1, value)).reduceLeft(_ + ",\n" + _) +
                "\n" + currentLevelOffset + "]"
            case JsonObject(obj) =>
                "{\n" + obj
                    .map(tuple => nextLevelOffset + "\"" + tuple._1 + "\": " + serialize(level + 1, tuple._2))
                    .reduceLeft(_ + ",\n" + _) +
                "\n" + currentLevelOffset + "}"
        }
    }

    val json = serialize(0, testJson)
    println(s"JSON\n\n" + json)

}
