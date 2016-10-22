object kr1 extends App {

/*  Для проверки работоспособности двух функций написаны тесты,
    находящиеся в test/scala/kr1Spec */

    /*set — функция, возвращающая список из всех атомов,
    содержащихся в заданном списке. Каждый атом должен
    присутствовать в результирующем списке в единственном числе.*/
    def set(list: List[Any]): List[Any] = {
        //Aналогично можно использовать - list.toSet.toList
        list.distinct
    }

    /*Eсли не пользоваться предопределённой функцией и преобразованием в Set,
    то функцию можно реализовать через изменяемый(mutable) список.*/
    def mutable_set(list: List[Any]): List[Any] = {
        val mutableList = scala.collection.mutable.ListBuffer.empty[Any]
        list.foreach(el => {
            if (!mutableList.contains(el)) mutableList += el
        })
        mutableList.toList
    }

    /*Или же через рекурсию, но с сохранением immutable state*/
    def immutable_set(list: List[Any]): List[Any] = list match {
        case x :: xs => x :: immutable_set(xs.filter(y => y != x))
        case Nil => list
    }

    /*freq — функция, возвращающая список пар (символ, частота).
    Каждая пара определяет атом из заданного списка и частоту его вхождения в этот список.*/
    def freq(list: List[Any]): List[(Any, Int)] = {
        set(list).zip(set(list).map(el => list.count(_ == el)))
    }

}
