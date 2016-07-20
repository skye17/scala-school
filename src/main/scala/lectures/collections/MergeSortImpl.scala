package lectures.collections

import scala.annotation.tailrec

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {
  @tailrec
  def tailMerge(left:Seq[Int], right:Seq[Int], acc:Seq[Int]):Seq[Int] = (left, right) match {
    case (Nil, y) => acc ++ y
    case (x, Nil) => acc ++ x
    case (lh::lt, rh::rt) =>
      if (lh < rh) tailMerge(lt, right, acc:+lh)
      else tailMerge(left, rt, acc:+rh)
  }

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    val n = data.length/2
    data match {
      case head :: Nil => data
      case head :: tail => tailMerge(mergeSort(data take n), mergeSort(data drop n), Nil)
    }
  }

  println(mergeSort(List(1,3, -2, 0, 5, 6, 6, -4, 2)))
}
