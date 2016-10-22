import org.scalatest.FlatSpec
import kr1.{set, freq}

class kr1Spec extends FlatSpec{

    val testList: List[Int] = List(1, 1, 1, 2, 2, 1, 2, 2, 3, 1, 2, 3, 3, 5, 3, 2, 1, 3)

    "Sum function" should "return no duplicate elements" in {
        assertResult(List(1, 2, 3, 5)) { set(testList) }
    }

    "Freq function" should "return list of pairs (element, frequency)" in {
        assertResult(List(
            (1, testList.count(_ == 1)),
            (2, testList.count(_ == 2)),
            (3, testList.count(_ == 3)),
            (5, testList.count(_ == 5)))
        ) { freq(testList) }
    }

}
