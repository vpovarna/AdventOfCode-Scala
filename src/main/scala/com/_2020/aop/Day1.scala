package com._2020.aop

import scala.annotation.tailrec

object Day1 {
  val input: String =
    """
      |1721
      |979
      |366
      |299
      |675
      |1456
      |""".stripMargin

  def solution1(numbers: List[Int], targetSum: Int): Int = {
    val (a, b) = twoSumTotalTailRec(numbers, targetSum)
    a * b
  }

  def solution2(numbers: List[Int], targetSum: Int): Int = {

    @tailrec
    def treeSumTailRec(numbers: List[Int], product: (Int, Int, Int) = (0, 0, 0)): (Int, Int, Int) = {
      numbers match {
        case head :: next =>
          val newTargetSum = targetSum - head
          val tuple = twoSumTotalTailRec(next, newTargetSum)
          if (tuple._1 != 0 & tuple._2 != 0) treeSumTailRec(next, (head, tuple._1, tuple._2))
          else treeSumTailRec(next, product)
        case Nil => product
      }
    }

    val (a, b, c) = treeSumTailRec(numbers)
    a * b * c
  }

  @tailrec
  def twoSumTotalTailRec(remainingNumber: List[Int], newTargetSum: Int, tuple: (Int, Int) = (0, 0)): (Int, Int) = {
    remainingNumber match {
      case head :: tail =>
        val result = newTargetSum - head
        if (remainingNumber.contains(result)) twoSumTotalTailRec(remainingNumber.tail, newTargetSum, (head, result))
        else twoSumTotalTailRec(tail, newTargetSum, tuple)
      case Nil => tuple
    }
  }

  def main(args: Array[String]): Unit = {
    val day1InputFile: String = "src/main/resources/2020/day1.txt"
    val targetSum = 2020
    println(s"The day1 part1.txt result is: ${solution1(readInputFileAsInt(day1InputFile), targetSum)}")
    println(s"The day1 part1.txt result is: ${solution2(readInputFileAsInt(day1InputFile), targetSum)}")
  }

}
