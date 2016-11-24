import java.io.{Reader, StringReader}

import scala.collection.mutable.ArrayBuffer

object lab2 extends App {

    abstract class Json
    case class JsonObject(value: (String, Json)*) extends Json
    case class JsonArray(value: Json*) extends Json
    case class JsonString(value: String) extends Json
    case class JsonNumber(value: Double) extends Json
    case class JsonBoolean(value: Boolean) extends Json
    case object JsonNull extends Json

    case class ParseJsonException(message: String, line: Int, column: Int)
        extends RuntimeException(s"$message at line $line, column $column")

    object Json {

        def read(text: String): Json = {
            val jsonString: List[List[Char]] = text.split('\n').map(line => line.toList).toList
            jsonString(1).foreach(println)
            read(new StringReader(text))
        }

        def read(reader: Reader): Json = {


            var line = 1
            var column = 0
            var current: Int = -1

            def next() = {
                if (current == '\n') {
                    line += 1
                    column = 1
                } else {
                    column += 1
                }
                current = reader.read()
                current
            }

            def skipWhitespace() = {
                while (Character.isWhitespace(current)) next()
            }

            next()

            def readObject(): Json = {
                if (current != '{')
                    throw ParseJsonException("Expected a '{'", line, column)
                next()
                skipWhitespace()
                val members = ArrayBuffer[(String, Json)]()
                var first = true
                while (current != '}') {
                    if (first) {
                        first = false
                    } else {
                        if (current != ',')
                            throw ParseJsonException("Expected a ','", line, column)
                        next()
                        skipWhitespace()
                    }
                    if (current != '"')
                        throw ParseJsonException("Expected a label", line, column)
                    val label = readString()
                    if (current != ':')
                        throw ParseJsonException("Expected a ':'", line, column)
                    next()
                    skipWhitespace()
                    val value = readJson()
                    members += (label -> value)
                }
                next()
                skipWhitespace()
                JsonObject(members: _*)
            }

            def readArray(): Json = {
                if (current != '[')
                    throw ParseJsonException("Expected an array '['", line, column)
                next()
                skipWhitespace()
                val elements = ArrayBuffer[Json]()
                var first = true
                while (current != ']') {
                    if (first) {
                        first = false
                    } else {
                        if (current != ',')
                            throw ParseJsonException("Expected a ','", line, column)
                        next()
                        skipWhitespace()
                    }
                    val value = readJson()
                    elements += value
                }
                next()
                skipWhitespace()
                JsonArray(elements: _*)
            }

            def readString(): String = {
                if (current != '"')
                    throw ParseJsonException("Expected a string", line, column)
                next()
                val builder = new StringBuilder()
                while (current != '"') {
                    if (current == -1)
                        throw ParseJsonException("Unexpected end of file inside a string", line, column)
                    if (current.toChar.isControl)
                        throw ParseJsonException("Unescaped control character inside a string", line, column)
                    if (current == '\\') {
                        val result = next() match {

                            case -1 => throw ParseJsonException(
                                "Unexpected end of file inside an escape sequence", line, column
                            )
                            case '"' => '"'
                            case '\\' => '\\'
                            case '/' => '/'
                            case 'b' => '\b'
                            case 'f' => '\f'
                            case 'n' => '\n'
                            case 'r' => '\r'
                            case 't' => '\t'
                            case 'u' =>

                                val hex1 = next()
                                val hex2 = next()
                                val hex3 = next()
                                val hex4 = next()
                                try {
                                    Integer.parseInt(
                                        "" + hex1.toChar + hex2.toChar + hex3.toChar + hex4.toChar, 16
                                    ).toChar
                                } catch {
                                    case e: NumberFormatException =>
                                        throw ParseJsonException(e.getMessage, line, column)
                                }

                            case _ =>
                                throw ParseJsonException("Unknown escape sequence: \\" + current.toChar, line, column)

                        }
                        builder.append(result)
                    } else {
                        builder.append(current.toChar)
                    }
                    next()
                }
                next()
                skipWhitespace()
                builder.toString()
            }

            def isNumberPart(c: Int): Boolean = {
                (c >= '0' && c <= '9') || c == '+' || c == '-' || c == 'e' || c == 'E' || c == '.'
            }

            def readNumber(): Json = {
                val builder = new StringBuilder
                while (isNumberPart(current)) {
                    builder.append(current.toChar)
                    next()
                }
                skipWhitespace()
                val text = builder.toString()
                if (text.startsWith(".") || text.startsWith("-.") || text.startsWith("+"))
                    throw ParseJsonException(
                        "A JSON number must have a digit before the . and can't start with a +", line, column
                    )
                try {
                    JsonNumber(java.lang.Double.parseDouble(text))
                } catch {
                    case e : NumberFormatException => throw ParseJsonException(e.getMessage, line, column)
                }
            }

            def readTrue(): Json = {
                if (current != 't') throw ParseJsonException("Expected 'true'", line, column)
                if (next() != 'r') throw ParseJsonException("Expected 'true'", line, column)
                if (next() != 'u') throw ParseJsonException("Expected 'true'", line, column)
                if (next() != 'e') throw ParseJsonException("Expected 'true'", line, column)
                next()
                skipWhitespace()
                JsonBoolean(value = true)
            }

            def readFalse() : Json = {
                if (current != 'f') throw ParseJsonException("Expected 'false'", line, column)
                if (next() != 'a') throw ParseJsonException("Expected 'false'", line, column)
                if (next() != 'l') throw ParseJsonException("Expected 'false'", line, column)
                if (next() != 's') throw ParseJsonException("Expected 'false'", line, column)
                if (next() != 'e') throw ParseJsonException("Expected 'false'", line, column)
                next()
                skipWhitespace()
                JsonBoolean(value = false)
            }

            def readNull() : Json = {
                if(current != 'n') throw ParseJsonException("Expected 'null'", line, column)
                if(next() != 'u') throw ParseJsonException("Expected 'null'", line, column)
                if(next() != 'l') throw ParseJsonException("Expected 'null'", line, column)
                if(next() != 'l') throw ParseJsonException("Expected 'null'", line, column)
                next()
                skipWhitespace()
                JsonNull
            }

            def readJson(): Json = {
                current match {
                    case '{' => readObject()
                    case '[' => readArray()
                    case '"' => JsonString(readString())
                    case 't' => readTrue()
                    case 'f' => readFalse()
                    case 'n' => readNull()
                    case c if isNumberPart(c) => readNumber()
                    case -1 => throw ParseJsonException(
                        "Unexpected end of file", line, column
                    )
                    case _ => throw ParseJsonException(
                        "Unexpected character: " + current.toChar + " (" + current + ")", line, column
                    )
                }
            }

            skipWhitespace()
            val result = readJson()
            result

        }

    }



    val result = Json.read(
        """{
                "id": "011A",
                "error": "Expected a ',' or '}' at 15 [character 16 line 1]",
                "price": "500\u00a3",
                "validate": false,
                "time": "03:53:25 AM",
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

    println(result)


}
