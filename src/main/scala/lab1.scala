import scala.math._

object lab1 extends App {

    type Result = (Double, Int)

    def getEtalonValue(x: Double) = -log(abs(2 * sin(x / 2)))
    def nthElem(n: Int, x: Double) = cos(n * x) / n
    val (steps, left, right) = (20, Pi / 5, 6 * Pi / 5)
    val step = (right - left) / steps
    val delta = 1e-3
    val epsilon = 1e-2
    val points = for (n <- left to right by step) yield n

    lazy val naturalNumbersStream: Stream[Int] = Stream from 1

    def taylor(x: Double, funcResult: Double): Result = {
        naturalNumbersStream.map(
            naturalNumbersStream.take(_)
                .map(nthElem(_, x))
                .sum
        ).zip(naturalNumbersStream)
            .find(elem => abs(elem._1 - funcResult) <= delta)
            .orNull
    }

    points.foreach(elem => {
        val funcResult: Double = getEtalonValue(elem)
        val res: Result = taylor(elem, funcResult)
        println("point: " + elem + " \tres: " + res._1 + " \tstep: " + res._2 + " \tvalue: " + funcResult)
    })



    //Task 2

    val functionsList: List[(Double) => Double] = List(
        (x: Double) => x - 2 + sin(1 / x),
        (x: Double) => exp(x) + log(x) - 10 * x,
        (x: Double) => cos(x) - exp(-pow(x, 2) / 2) + x - 1
    )

    val functionsDerivativesList: List[(Double) => Double] = List(
        (x: Double) => 1 - cos(1 / x) / pow(x, 2),
        (x: Double) => exp(x) + 1 / x - 10,
        (x: Double) => x * exp(-pow(x, 2) / 2) - sin(x) + 1
    )

    val bordersList: List[(Double, Double)] = List(
        (1.2, 2.0),
        (3.0, 4.0),
        (1.0, 2.0)
    )

    val preciseValuesList: List[Double] = List(1.3077, 3.5265, 1.0804)

    val summaryList = ((functionsList zip functionsDerivativesList) zip bordersList) zip preciseValuesList map {
        case (((x, y), z), k) => (x, y, z, k)
    }

    case class NewtonMethod(f: Double => Double, df: Double => Double) {
        def getRoot(initialValue: Double, preciseValue: Double): Result = {
            def nextX(x: Double) = x - f(x) / df(x)
            lazy val newtonStream: Stream[Double] =
                Stream.cons(initialValue, newtonStream.map(xx => nextX(xx)))
            newtonStream.zip(naturalNumbersStream)
                .find(tuple => abs(nextX(tuple._1) - tuple._1) < delta)
                .orNull
        }
    }

    for ((f, fd, r, value) <- summaryList) {
        println("Newton method: " + '\t' + NewtonMethod(f, fd).getRoot((r._1 + r._2) / 2, value))
    }

    case class DichotomyMethod() {
        def getRoot(f: Double => Double, value: Double)(borders: (Double, Double), step: Int): (Double, Int) = {
            lazy val dichFunc: ((Double, Double), Int) => (Double, Int) = getRoot(f, value)
            lazy val middlePoint: Double = (borders._1 + borders._2) / 2
            if (abs(middlePoint - value) <= epsilon) (middlePoint, step)
            else {
                if (f(borders._1) * f(middlePoint) < 0) {
                    dichFunc((borders._1, middlePoint), step + 1)
                } else {
                    dichFunc((middlePoint, borders._2), step + 1)
                }
            }
        }
    }

    for ((f, fd, borders, value) <- summaryList) {
        println("Dichotomy method: " + '\t' + DichotomyMethod().getRoot(f, value)(borders, 1))
    }

    case class IterativeMethod(f: Double => Double) {
        def getRoot(initialValue: Double): Result = {
            def nextX(x: Double) = f.apply(x)
            lazy val iterativeStream: Stream[Double] =
                Stream.cons(initialValue, iterativeStream.map(xx => nextX(xx)))
            iterativeStream.zip(naturalNumbersStream)
                .find(tuple => abs(nextX(tuple._1) - tuple._1) < delta)
                .orNull
        }
    }

    //Подбор ф-ций таким образом, чтобы производная по модулю на всём интервале была < 1
    val iterativeMethodFunctionsTransformationList: List[(Double) => Double] = List(
        (x: Double) => 2 - sin(1 / x),
        (x: Double) => log(10 * x + log(x)),
        (x: Double) => 1 + exp(-pow(x, 2) / 2) - cos(x)
    )

    val iterativeSummaryList = (iterativeMethodFunctionsTransformationList, bordersList).zipped.toList

    for ((f, borders) <- iterativeSummaryList) {
        println("Iterative method: " + '\t' + IterativeMethod(f).getRoot((borders._1 + borders._2) / 2))
    }

}
