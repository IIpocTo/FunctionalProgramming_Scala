import lab2._
import org.scalatest.FlatSpec

class lab2Spec extends FlatSpec {

    private val json: String =
        """{
                "id": "011A"
        }"""

    "Json parser" should "generate tokens" in {
        assertResult(List(
            OpenCurlyBracket, StringToken("id"), Colon, StringToken("011A"), CloseCurlyBracket
        ))(getTokens(json))
    }

    it should "parse tokens to json object" in {
        assertResult(JsonObject(List(("id",JsonString("011A")))))(parseTokens(getTokens(json)))
    }

    it should "calculate hash" in {
        assertResult(Map(
            "JsonNull" -> 0,
            "JsonString" -> 1,
            "JsonArray" -> 0,
            "JsonObject" -> 1,
            "JsonBoolean" -> 0,
            "JsonNumber" -> 0
        ))(calculateHash(parseTokens(getTokens(json))))
    }

    "Both jsons" should "be equal in case of randomly generated json" in {
        val randomJsonObject = generateRandomJsonObject(5)
        assertResult(randomJsonObject)(parseTokens(getTokens(serialize(randomJsonObject))))
    }

}
