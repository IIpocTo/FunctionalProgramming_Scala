import org.scalatest.FlatSpec

import kr2._

class kr2Spec extends FlatSpec {

    val first20TriangleNumbers: List[Int] =
        List(0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 171, 190)

    val first20PyramidalNumbers: List[Int] =
        List(0, 1, 5, 14, 30, 55, 91, 140, 204, 285, 385, 506, 650, 819, 1015, 1240, 1496, 1785, 2109, 2470)

    "Function getTriangleNumbers" should "return right lazy stream of triangle numbers" in {
        assertResult(first20TriangleNumbers) {getTriangleNumbersList(20)}
    }

    "Function getPyramidalNumbers" should "return right lazy stream of pyramidal numbers" in {
        assertResult(first20PyramidalNumbers) {getPyramidalNumbersList(20)}
    }

}
