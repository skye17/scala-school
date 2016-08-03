package lectures.oop

import scala.util.Random


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения(null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl.
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в  условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужгно раскомментировать и реадизовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]

  def traverse:List[Int]
  def traverseBFS:List[List[Int]]
  def fold(aggregator: Int)(f: (Int, Int) =>(Int)):Int
  // Apply some function to each level
  def foldBFS(neutral:Int)(f:(Int, Int) => Int):List[Int]
}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {

  def addImpl(newValue: Int):BSTImpl = {
    if (newValue < value) {
      left match {
        case Some(tree:BSTImpl) => BSTImpl(value, Some(tree.addImpl(newValue)), right)
        case None => BSTImpl(value, Some(BSTImpl(newValue)),right)
      }
    }
    else if (newValue > value) {
      right match {
        case Some(tree:BSTImpl) => BSTImpl(value, left, Some(tree.addImpl(newValue)))
        case None => BSTImpl(value, left,Some(BSTImpl(newValue)))
      }
    } else this

  }

  def add(newValue: Int): BST = addImpl(newValue)

  private def findImpl(value: Int): Option[BSTImpl] =
    if (value == this.value) Some(this)
    else
      if (value < this.value)
        left match {
          case Some(tree) => tree.findImpl(value)
          case None => None
        }
      else {
        right match {
          case Some(tree) => tree.findImpl(value)
          case None => None
        }
      }


  //def find(value: Int): Option[BST] = findImpl(value)
  def find(value:Int):Option[BST] = if (traverseBFS.exists(level => level.contains(value))) Some(this) else None



  // override def toString() = ???

  def traverse:List[Int] = traverseAcc(Nil)
  private def traverseAcc(acc:List[Int]):List[Int] = {
    (left, right) match {
      case (None,None) => acc :+ value
      case (Some(tree), None) => tree.traverse ++ List(value) ++ acc
      case (None, Some(tree)) => List(value) ++ tree.traverse ++ acc
      case (Some(leftTree), Some(rightTree)) => leftTree.traverse ++ List(value) ++ rightTree.traverse ++ acc
    }
  }

  def traverseBFS:List[List[Int]] = {
    def merge(a:List[List[Int]], b:List[List[Int]], acc:List[List[Int]] = Nil):List[List[Int]] =
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

  def fold(aggregator: Int)(f: (Int, Int) =>(Int)):Int = traverse.fold(aggregator)(f)

  def foldBFS(neutral:Int)(f: (Int, Int) =>(Int)) = traverseBFS.map(_.foldLeft(neutral)(f))
}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = randomTreeGenerator(nodesCount) // generator goes here

  val seqTree = seqTreeGenerator(nodesCount)
  println(seqTree)
  println(seqTree.fold(0)(_+_))

  //println(seqTree.traverseBFS)
  //println(seqTree.foldBFS(0)(_+_))

  for (i <- 1 to nodesCount) {assert(seqTree.find(i).isDefined)}
  assert(seqTree.find(0).isEmpty)





  def seqTreeGenerator(numNodes:Int): BST = {
    val treeValues = Random.shuffle(1 to numNodes toList)
    treeValues.tail.foldLeft(new BSTImpl(treeValues.head))((tree:BSTImpl, newVal:Int) => tree.addImpl(newVal))
  }

  def randomTreeGenerator(numNodes:Int): BST = {
    val treeValues = for ( i <- 1 until numNodes) yield (Math.random()*maxValue).toInt
    treeValues.foldLeft(BSTImpl((Math.random()*maxValue).toInt))((tree:BSTImpl, newVal:Int) => tree.addImpl(newVal))
  }
  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)
  val addedRandom = randomTreeGenerator(nodesCount).add(markerItem).add(markerItem2).add(markerItem3)
  val addedSeqTree = seqTree.add(markerItem).add(markerItem2).add(markerItem3)

  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(addedRandom.find(markerItem).isDefined)
  require(addedSeqTree.find(markerItem).isDefined)
  require(testTree.find(markerItem2).isDefined)
  require(addedRandom.find(markerItem2).isDefined)
  require(addedSeqTree.find(markerItem2).isDefined)
  require(testTree.find(markerItem3).isDefined)
  require(addedRandom.find(markerItem3).isDefined)
  require(addedSeqTree.find(markerItem3).isDefined)

  //println(testTree)

}