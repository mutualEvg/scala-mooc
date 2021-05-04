package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] = currentLine
    .foldLeft(List.empty[Count])(foldElements)
    .reverse
    .flatMap(a => a.flatten)

  def foldElements(list: List[Count], digit: Int): List[Count] = {
    if (list.isEmpty) {
      List(Count(digit))
    } else {
      if (list.head.digit == digit) {
        list.head.increment :: list.tail
      } else {
        Count(digit) :: list
      }
    }
  }

  case class Count(digit: Int, count: Int = 1) {
    def increment: Count = copy(count = count + 1)
    def flatten: List[Int] = List(count, digit)
  }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = LazyList.iterate(List(1))(nextLine)
}