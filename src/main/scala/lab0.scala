//import com.stackmob.newman.ApacheHttpClient
//import com.stackmob.newman.dsl._
//import com.stackmob.newman.response.HttpResponse
//
//import scala.concurrent.Await
//import scala.concurrent.duration._
//
//object lab0 extends App {
//
//    def pascalTriangle(col: Int, row: Int): Int = {
//        if (col == 0 || col == row) 1
//        else pascalTriangle(col - 1, row - 1) + pascalTriangle(col, row - 1)
//    }
//
//    def getPascalList(n: Int) = {
//        for (x <- 0 to n; y <- 0 to x) yield pascalTriangle(y, x)
//    }
//
//    val contentString = "[" + getPascalList(20).mkString(",") + "]"
//
//    println(contentString)
//
//    implicit val httpClient = new ApacheHttpClient
//
//    val response: HttpResponse = Await.result(POST(url(http, "91.239.142.110", 13666) / "lab0")
//        .setHeaders("Content-Type" -> "application/x-www-form-urlencoded")
//        .setBody(s"email=allxf95@gmail.com&name=Filshin A. A.&content=$contentString")
//        .apply, 2.seconds)
//    println(response.bodyString)
//}
