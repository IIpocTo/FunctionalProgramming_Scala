import lab2._
import org.scalatest.FlatSpec

class Lab2Spec extends FlatSpec {

    private val json: String =
        """{
                "id": "011A"
        }"""

    "Both jsons" should "be equal in case of randomly generated json" in {
        val randomJsonObject = generateRandomJsonObject(5)
        assertResult(randomJsonObject)(parseTokens(getTokens(serialize(randomJsonObject))))
    }

    "Json parser" should "generate tokens" in {
        assertResult(List(
            OpenCurlyBracket, StringToken("id"), Colon, StringToken("011A"), CloseCurlyBracket
        ))(getTokens(json))
    }

    it should "parse tokens to json object" in {
        assertResult(JsonObject(List(("id",JsonString("011A")))))(parseTokens(getTokens(json)))
    }

}
