package lectures.collections

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Представим, что по какой-то причине, Вам понадобилась своя обертка над списком целых чисел List[Int]
  *
  * Вы приняли решение, что будет достаточно реализовать 4 метода
  * * * * * def flatMap(f: (Int => MyList)) -  реализуете на основе соответствующего метода из List
  * * * * * метод map(f: (Int) => Int) - с помощью только что полученного метода flatMap класса MyList
  * * * * * filter(???) - через метод flatMap класса MyList
  * * * * * foldLeft(acc: Int)(???) - через декомпозицию на head и tail
  *
  * Для того, что бы выполнить задание,
  * * * * * раскомментируйте код
  * * * * * замените знаки вопроса на сигнатуры и тела методов
  * * * * * не используйте var и мутабильные коллекции
  *
  */
object MyListImpl extends App {

  case class MyList[T, M <: Iterable[T]](data: Iterable[T]) {

    def flatMap(f: (T => MyList[T, M])): MyList[T, Iterable[T]] =
      new MyList(data.flatMap(inp => f(inp).data))

    //
    def map(f: (T => T)): MyList[T, Iterable[T]] = flatMap(inp => new MyList(List(f(inp))))

    //
    def foldLeft(acc: T)(f: ((T, T)) => T): T = data match {
      case Nil => acc
      case head::tail => new MyList(tail).foldLeft(f(acc, head))(f)
    }

    def filter(f: (T => Boolean)): MyList[T, Iterable[T]] = flatMap(x => if (f(x)) new MyList(List(x)) else new MyList(List()))
  }


  class MyListBuffer[T](override val data:ListBuffer[T]) extends MyList(data)

  class MyIndexedList[T](data:IndexedSeq[T]) extends MyList(data)

  /*require(MyList(List(1, 2, 3, 4, 5, 6)).map(_ * 2).data == List(2, 4, 6, 8, 10, 12))
  require(MyList(List(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyList(List(1, 2, 3, 4, 5, 6)).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 21)
  require(MyList(Nil).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)*/

  require(MyList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).map(p => p * 2).data == List(2, 4, 6, 8, 10, 12))
  require(MyList[Long, ListBuffer[Long]](ListBuffer(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 21)
  require(MyList[Float, IndexedSeq[Float]](ArrayBuffer.empty[Float]).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)


  require(new MyListBuffer[Long](ListBuffer(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(new MyIndexedList[Float](ArrayBuffer.empty[Float]).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)
}

trait Numeric[T] {
  def add(x: T, y: T): T
}





object NumericExperiment extends App{
  implicit val int2Num: Numeric[Int] = new Numeric[Int]() {
    override def add(x: Int, y: Int): Int = x + y
  }

  def doAdd[T: Numeric](one: T, other: T) = {
    val r = implicitly[Numeric[T]]
    println("Added " + r.add(one, other))
  }

  def doAddWithExplicitPrm[T](one: T, other: T)(implicit evidence: Numeric[T]) = {
    println("Added " + evidence.add(one, other))
  }

  case class MoneyAmount(money: Double)

  implicit val money2Num: Numeric[MoneyAmount] = new Numeric[MoneyAmount]() {
    override def add(x: MoneyAmount, y: MoneyAmount): MoneyAmount = MoneyAmount(x.money +
      y.money)
  }


  doAdd(MoneyAmount(100), MoneyAmount(300))

  //doAdd(1, 2)
  //doAddWithExplicitPrm(1, 2)
}