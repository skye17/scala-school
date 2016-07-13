package lectures.operators

import lectures.functions.{FunctionalComputation, CurriedComputation, Computation, Data}

/**
  * В задачке из lectures.functions.Computations, мы реализовали
  * один и тот же метод 3-я разными способами
  *
  * Пришло время оценить на сколько разные имплементации
  * отличаются друг от друга по производительности
  *
  * Для этого, раскомментируйте код, выполните в циклах вызов 3-х имплементаций
  * Оцените разницу во времени выполнения и объясните ее происхожение
  *
  */


object EvaluateOptimization extends App with Data {

  val startTimestamp = System.currentTimeMillis()
  // ВЫПОЛНИТЬ В ЦИКЛЕ  ОТ 1 ДО 100 Computation.computation(
      for(i <- 1 to 100) {
        val f = Computation.computation(filterData, dataArray)
        println("elapsed time in Computation.computation" + (System.currentTimeMillis() - startTimestamp))
     }

  val start1Timestamp = System.currentTimeMillis()
  // ВЫПОЛНИТЬ В ЦИКЛЕ  ОТ 1 ДО 100 CurriedComputation.partiallyAppliedCurriedFunction(
      for(j <- 1 to 100 ) {
        //println(dataArray)
        CurriedComputation.partiallyAppliedCurriedFunction(dataArray)
      println("elapsed time in Curried" + (System.currentTimeMillis() - start1Timestamp))
     }
  val start2Timestamp = System.currentTimeMillis()

  // ВЫПОЛНИТЬ В ЦИКЛЕ  ОТ 1 ДО 100 FunctionalComputation.filterApplied
     for(i <- 1 to 100) {
       val f = FunctionalComputation.filterApplied(dataArray)
      println("elapsed time in Functional" + (System.currentTimeMillis() - start2Timestamp))
     }
  val end2Timestamp = System.currentTimeMillis()

  // ВЫВЕСТИ РАЗНИЦУ В ПРОДОЛЖИТЕЛЬНОСТИ ВЫПОЛНЕНИЯ МЕЖДУ КАРРИРОВАННОЙ ВЕРСИЕЙ
  // И ФУНКЦИОНАЛЬНОЙ

    val diff = start2Timestamp - start1Timestamp - (end2Timestamp - start2Timestamp)

    print(s"Difference is about $diff milliseconds")
}

