package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {
  def merge(left:Seq[Int], right:Seq[Int]):Seq[Int] = (left, right) match {
    case (Nil, y) => y
    case (x, Nil) => x
    case _ =>
      if (left.head < right.head) left.head+:merge(left.tail, right)
      else right.head+:merge(left, right.tail)
  }

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    val n = data.length/2
    data match {
      case head :: Nil => data
      case head :: tail => merge(mergeSort(data take n), mergeSort(data drop n))
    }
  }

  println(mergeSort(List(1,3, -2, 0, 5, 6, 6, -4, 2)))
}
