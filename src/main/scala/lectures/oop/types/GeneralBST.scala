package lectures.oop.types


import scala.math.Ordering.{Float, String}

import lectures.matching.SortingStuff.Watches

import scala.util.Random

/**
  * Модифицируйте реализацию BSTImpl из предыдущего задания.
  * Используя тайп параметры и паттерн Type Class реализуте GeneralBSTImpl таким образом,
  * что бы дерево могло работать с произвольным типом данных
  *
  * Наслеников GeneralBSTImpl определять нельзя
  *
  * Создайте генератор для деревьев 3-х типов данных
  * * * * float
  * * * * String
  * * * * Watches из задачи SortStuff. Большими считаються часы с большей стоимостью
  */

trait GeneralBST[T] {
  val value: T
  val left: Option[GeneralBST[T]]
  val right: Option[GeneralBST[T]]

  def add(newValue: T): GeneralBST[T]

  def traverseBFS:List[List[T]]

  def find(value: T): Option[GeneralBST[T]]
}

case class GeneralBSTImpl[T](value:T, left: Option[GeneralBST[T]] = None,
                             right: Option[GeneralBST[T]] = None)(implicit val ordering: Ordering[T]) extends GeneralBST[T] {
  //override val value = _
  implicit  val o = scala.math.Ordering[Float]

  override def find(value: T): Option[GeneralBST[T]] = if (traverseBFS.exists(level => level.contains(value))) Some(this) else None

  def addImpl(newValue: T): GeneralBSTImpl[T] =  {
    if (ordering.compare(newValue,value) < 0) {
      left match {
        case Some(tree:GeneralBST[T]) => GeneralBSTImpl(value, Some(tree.add(newValue)), right)
        case None => GeneralBSTImpl(value, Some(GeneralBSTImpl(newValue)),right)
      }
    }
    else if (ordering.compare(newValue,value) > 0) {
      right match {
        case Some(tree:GeneralBST[T]) => GeneralBSTImpl(value, left, Some(tree.add(newValue)))
        case None => GeneralBSTImpl(value, left,Some(GeneralBSTImpl(newValue)))
      }
    } else this
  }

  override def add(newValue:T):GeneralBST[T] = addImpl(newValue)


  def traverseBFS:List[List[T]] = {
    def merge(a:List[List[T]], b:List[List[T]], acc:List[List[T]] = Nil):List[List[T]] =
      (a, b) match {
        case (Nil,Nil) => acc
        case (head::_, Nil) => acc ++ a
        case (Nil, head::_) => acc ++ b
        case (leftHead::leftTail, rightHead::rightTail) => merge(leftTail, rightTail, acc :+ (leftHead ++ rightHead))
      }
    (left, right) match {
      case (None,None) => List(List(value))
      case (Some(tree), None) => List(value) +: tree.traverseBFS
      case (None, Some(tree)) => List(value) +: tree.traverseBFS
      case (Some(leftTree), Some(rightTree)) => List(value) +: merge(leftTree.traverseBFS, rightTree.traverseBFS)

    }
  }

  //override val left: Option[GeneralBST] = ???
  //override val right: Option[GeneralBST] = ???

}

object GeneralTreeTest extends App {
  val maxValue = 5000
  implicit val watchOrdering:Ordering[Watches] = Ordering.by((w:Watches) => w.cost)


  def floatTreeGenerator(numNodes:Int): GeneralBST[Float] = {
    val treeValues = for ( i <- 1 until numNodes) yield (Math.random()* maxValue).toFloat
    treeValues.foldLeft(GeneralBSTImpl((Math.random()*maxValue).toFloat))((tree:GeneralBSTImpl[Float], newVal:Float) => tree.addImpl(newVal))
  }

  val floatTree = floatTreeGenerator(5)
  val strTree = stringTreeGenerator(5)
  val watchTree = watchTreeGenerator(3)
  println(floatTree)
  println
  println(strTree)
  println
  println(watchTree)
  println

  def stringGenerator(n:Int):String = Random.alphanumeric.take(n).mkString

  def stringTreeGenerator(numNodes:Int): GeneralBST[String] = {
    val treeValues = for ( i <- 1 until numNodes) yield stringGenerator(5)
    treeValues.foldLeft(GeneralBSTImpl(stringGenerator(5)))((tree:GeneralBSTImpl[String], newVal:String) => tree.addImpl(newVal))
  }

  def watchTreeGenerator(numNodes:Int): GeneralBST[Watches] = {
    val treeValues = for ( i <- 1 until numNodes) yield Watches(stringGenerator(10), (Math.random()*maxValue).toFloat)
    treeValues.foldLeft(GeneralBSTImpl(Watches(stringGenerator(10), (Math.random()*maxValue).toFloat)))((tree:GeneralBSTImpl[Watches], newVal:Watches) => tree.addImpl(newVal))
  }

}
