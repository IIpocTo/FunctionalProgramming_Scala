import java.nio.file.Paths

import com.norbitltd.spoiwo.model.enums.{CellFill, CellHorizontalAlignment}
import com.norbitltd.spoiwo.model._
import com.norbitltd.spoiwo.natures.xlsx.Model2XlsxConversions._
import com.stackmob.newman.ApacheHttpClient
import com.stackmob.newman.dsl._
import com.stackmob.newman.response.HttpResponse

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.math._

object lab1 extends App {

    type Result = (Double, Int)

    def getEtalonValue(x: Double) = -log(abs(2 * sin(x / 2)))
    def nthElem(n: Int, x: Double) = cos(n * x) / n
    val (steps, left, right) = (20, Pi / 5, 6 * Pi / 5)
    val step = (right - left) / steps
    val delta = 1e-3
    val epsilon = 1e-10
    val points = for (n <- left to right by step) yield n

    lazy val naturalNumbersStream: Stream[Int] = Stream from 1

    // Task 1

    def taylor(x: Double, funcResult: Double): Result = {
        naturalNumbersStream.map(
            naturalNumbersStream.take(_)
                .map(nthElem(_, x))
                .sum
        ).zip(naturalNumbersStream)
            .find(elem => abs(elem._1 - funcResult) <= delta)
            .orNull
    }

    //выводит результат в консоль и в Excel файл по передаваемому пути
    def printTaylorResult(path: String): Unit = {

        lazy val resList = for (point <- points) yield {
            val funcResult: Double = getEtalonValue(point)
            val res: Result = taylor(point, funcResult)
            println("point: " + point + " \tresult: " + res._1 + " \tstep: " + res._2 + " \tfunc_value: " + funcResult)
            (point, res._1, res._2, funcResult)
        }

        val resultRows = resList.map(tuple =>
            Row().withCellValues(tuple._1, tuple._2, tuple._3, tuple._4)).toList

        val headerStyle = CellStyle(
            font = Font(bold = true),
            horizontalAlignment = CellHorizontalAlignment.Center
        )

        val rows = List(Row(style = headerStyle)
            .withCellValues("point", "res", "step", "value", "precision = 1e-3")) ::: resultRows

        val createSheet: Sheet = Sheet(name = "Taylor function results")
            .withRows(rows)
            .withColumns(
                Column(index = 0, style = CellStyle(font = Font(bold = true)), autoSized = true),
                Column(index = 1, style = CellStyle(font = Font(bold = true)), autoSized = true),
                Column(index = 2, style = CellStyle(font = Font(bold = true)), autoSized = true),
                Column(index = 3, style = CellStyle(font = Font(bold = true)), autoSized = true),
                Column(index = 4, style = CellStyle(font = Font(bold = true)), autoSized = true)
            )

        createSheet.saveAsXlsx(path)

    }


    //Task 2

    val functionsList: List[(Double) => Double] = List(
        (x: Double) => x - 2 + sin(1 / x),
        (x: Double) => exp(x) + log(x) - 10 * x,
        (x: Double) => cos(x) - exp(-pow(x, 2) / 2) + x - 1
    )

    val functionsStringList: List[String] = List(
        "x - 2 + sin(1 / x) = 0",
        "exp(x) + ln(x) - 10x = 0",
        "cos(x) - exp(-x ^ 2 / 2) + x - 1 = 0"
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

    val summaryList = (functionsList, functionsDerivativesList, bordersList).zipped.toList

    //Подбор ф-ций таким образом, чтобы производная по модулю на всём интервале была < 1
    val iterativeMethodFunctionsTransformationList: List[(Double) => Double] = List(
        (x: Double) => 2 - sin(1 / x),
        (x: Double) => log(10 * x - log(x)),
        (x: Double) => 1 + exp(-pow(x, 2) / 2) - cos(x)
    )

    val iterativeSummaryList = (iterativeMethodFunctionsTransformationList, bordersList).zipped.toList

    case class NewtonMethod(f: Double => Double, df: Double => Double) {
        def getRoot(initialValue: Double): Result = {
            def nextX(x: Double) = x - f(x) / df(x)
            lazy val newtonStream: Stream[Double] =
                Stream.cons(initialValue, newtonStream.map(xx => nextX(xx)))
            newtonStream.zip(naturalNumbersStream)
                .find(tuple => abs(nextX(tuple._1) - tuple._1) <= epsilon)
                .orNull
        }
    }

    case class DichotomyMethod() {
        def getRoot(f: Double => Double)(borders: (Double, Double), step: Int): (Double, Int) = {
            lazy val dichFunc: ((Double, Double), Int) => (Double, Int) = getRoot(f)
            lazy val middlePoint: Double = (borders._1 + borders._2) / 2
            if (abs(borders._2 - borders._1) <= epsilon) (middlePoint, step)
            else {
                if (f(borders._1) * f(middlePoint) < 0) {
                    dichFunc((borders._1, middlePoint), step + 1)
                } else {
                    dichFunc((middlePoint, borders._2), step + 1)
                }
            }
        }
    }

    case class IterativeMethod(f: Double => Double) {
        def getRoot(initialValue: Double): Result = {
            def nextX(x: Double) = f.apply(x)
            lazy val iterativeStream: Stream[Double] =
                Stream.cons(initialValue, iterativeStream.map(xx => nextX(xx)))
            iterativeStream.zip(naturalNumbersStream)
                .find(tuple => abs(nextX(tuple._1) - tuple._1) <= epsilon)
                .orNull
        }
    }

    //выводит результат в консоль и в Excel файл по передаваемому пути
    def printNumericMethodsResult(path: String): Unit = {

        val newtonResultList: List[((Double, Int), String)] =
            (for ((f, df, border) <- summaryList) yield {
                lazy val newtonMethodRes: (Double, Int) = NewtonMethod(f, df).getRoot((border._1 + border._2) / 2)
                println("Newton method: " + '\t' + newtonMethodRes)
                newtonMethodRes
            }, functionsStringList).zipped.toList

        val dichotomyResultList: List[((Double, Int), String)] =
            (for ((f, df, borders) <- summaryList) yield {
                val dichotomyMethodRes: (Double, Int) = DichotomyMethod().getRoot(f)(borders, 1)
                println("Dichotomy method: " + '\t' + dichotomyMethodRes)
                dichotomyMethodRes
            }, functionsStringList).zipped.toList

        val iterativeResultList: List[((Double, Int), String)] =
            (for ((f, borders) <- iterativeSummaryList) yield {
                val iterativeMethodRes: (Double, Int) = IterativeMethod(f).getRoot((borders._1 + borders._2) / 2)
                println("Iterative method: " + '\t' + iterativeMethodRes)
                iterativeMethodRes
            }, functionsStringList).zipped.toList

        val headerStyle = CellStyle(
            fillForegroundColor = Color.AquaMarine,
            fillBackgroundColor = Color.AquaMarine,
            font = Font(bold = true),
            horizontalAlignment = CellHorizontalAlignment.Center,
            fillPattern = CellFill.Solid
        )

        val centerAlignStyle: CellStyle = CellStyle(
            horizontalAlignment = CellHorizontalAlignment.Center
        )

        val centerAndFormatAlignStyle: CellStyle = CellStyle(
            horizontalAlignment = CellHorizontalAlignment.Center,
            dataFormat = CellDataFormat("##.##############")
        )

        val newtonResultRows = newtonResultList.map {
            tuple => Row(style = centerAlignStyle).withCells(
                Cell(tuple._2, style = centerAlignStyle),
                Cell(tuple._1._1, style = centerAndFormatAlignStyle),
                Cell(tuple._1._2, style = centerAlignStyle)
            )
        }
        val dichotomyResultRows = dichotomyResultList.map {
            tuple => Row(style = centerAlignStyle).withCells(
                Cell(tuple._2, style = centerAlignStyle),
                Cell(tuple._1._1, style = centerAndFormatAlignStyle),
                Cell(tuple._1._2, style = centerAlignStyle)
            )
        }
        val iterativeResultRows = iterativeResultList.map {
            tuple => Row(style = centerAlignStyle).withCells(
                Cell(tuple._2, style = centerAlignStyle),
                Cell(tuple._1._1, style = centerAndFormatAlignStyle),
                Cell(tuple._1._2, style = centerAlignStyle)
            )
        }

        lazy val headerRow = Row(style = centerAlignStyle).withCellValues("function", "result", "step")
        val rows =
            List(
                Row(style = headerStyle).withCellValues("Newton Method", "precision = 1e-10"),
                headerRow) ::: newtonResultRows :::
            List(
                Row(style = headerStyle).withCellValues("Dichotomy Method", "precision = 1e-10"),
                headerRow) ::: dichotomyResultRows :::
            List(
                Row(style = headerStyle).withCellValues("Iterative Method", "precision = 1e-10"),
                headerRow) ::: iterativeResultRows

        val createSheet: Sheet = Sheet(name = "Numeric methods results")
            .withRows(rows)
            .withColumns(
                Column(index = 0, style = CellStyle(font = Font(bold = true)), autoSized = true),
                Column(index = 1, style = CellStyle(font = Font(bold = true)), autoSized = true),
                Column(index = 2, style = CellStyle(font = Font(bold = true)), autoSized = true)
            )

        createSheet.saveAsXlsx(path)

    }

    printTaylorResult("C:\\Temp\\mephiFP_taylor.xlsx")
    printNumericMethodsResult("C:\\Temp\\mephiFP_numMethods.xlsx")

    val currentDirectory: String = Paths.get("").toAbsolutePath.toString
    val filePath: String = currentDirectory + "\\src\\main\\scala\\lab1.scala"

    val source = Source.fromFile(filePath)
    val content = try source.mkString finally source.close()

    //Sending results

    implicit val httpClient = new ApacheHttpClient
    val email = "allxf95@gmail.com"

    val response: HttpResponse = Await.result(POST(url(http, "91.239.142.110", 13666) / "lab1")
        .setHeaders("Content-Type" -> "application/x-www-form-urlencoded")
        .setBody(s"email=$email&content=$content")
        .apply, 2.seconds)
    println(response.bodyString)

}
