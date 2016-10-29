object kr2 extends App {

    /**
      * @return Ленивый поток вычисляющий натуральные числа.
      */
    def getNaturalNumbers: Stream[BigInt] = {
        lazy val naturalNumbersStream: Stream[BigInt] =
            Stream.cons(BigInt(1), naturalNumbersStream.map(_ + 1))
        naturalNumbersStream
    }

    /**
      * Треугольное число T — это число кружков, которые
      * могут быть расставлены в форме правильного треугольника.
      *
      * Пример: T(4)
      *     x
      *    x x
      *   x x x
      *  x x x x
      *
      * С чисто арифметической точки зрения,
      * n-е треугольное число — это сумма n первых натуральных чисел.
      *
      * Рекуррентная формула: T(N) = T(N - 1) + N
      *
      * @usecase Вычисление списка строится путём составления ленивого потока
      *          вычисления нужных чисел. Берётся первое число - 0. Следующее число
      *          получаем составляя кортеж (предыдущее число, номер нового числа)
      *          и просто складывая два его элемента. Далее просто берём из полученнгого
      *          бесконечного потока n нужных нам чисел.
      *
      * @param n требуемое число треугольных чисел.
      * @return Список из первых n треугольных чисел.
      * @example getTriangleNumbersList(3) = List(0, 1, 3)
      */
    def getTriangleNumbersList(n: Int): List[BigInt] = {
        lazy val triangleNumbersStream: Stream[BigInt] =
            Stream.cons(BigInt(0), triangleNumbersStream.zip(getNaturalNumbers).map(a => a._1 + a._2))
        triangleNumbersStream.take(n).toList
    }

    /**
      * Пирамидальное число P — число, представляющее собой количество
      * сложенных сфер в пирамиде с квадратным основанием. Квадратные пирамидальные числа
      * также выражают количество квадратов со сторонами, параллельными осям координат, в сетке N × N.
      *
      * Рекуррентная формула: P(N) = P(N - 1) + N^2^
      *
      * @usecase Вычисление списка строится путём составления ленивого потока
      *          вычисления нужных чисел. Берётся первое число - 0. Следующее число
      *          получаем составляя кортеж (предыдущее число, номер нового числа)
      *          и просто складывая предыдущее число с квадратом номера нового.
      *          Далее просто берём из полученнгого бесконечного потока n нужных нам чисел.
      *
      * @param n требуемое число пирамидальных чисел.
      * @return Список из первых n пирамидальных чисел.
      * @example getPyramidalNumbersList(14) = List(0, 1, 5, 15)
      */
    def getPyramidalNumbersList(n: Int): List[BigInt] = {
        lazy val pyramidalNumbersStream: Stream[BigInt] =
            Stream.cons(BigInt(0), pyramidalNumbersStream.zip(getNaturalNumbers).map(a => a._1 + a._2.pow(2)))
        pyramidalNumbersStream.take(n).toList
    }

}
