package com._2020.aop

import scala.annotation.tailrec

object Day5 {

  def main(args: Array[String]): Unit = {

    val inputStr = List("BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL")
    println(s"Test input data solution is: ${getSeats(inputStr, 8).max}")

    val day5Input = readInputFileAsString("src/main/resources/2020/day5.txt")
    println(s"[Part1] Day5 input data solution is: ${getSeats(day5Input).max}")

    val sortedSeats = getSeats(day5Input, 8).sorted
    println(s"[Part2] Day5 input data solution is: ${(sortedSeats.min to sortedSeats.max).diff(sortedSeats).head}")
  }

  @tailrec
  private def getNumber(str: String, startIdx: Int, endIdx: Int, acc: Int = 0): Int = {
    if (str.isEmpty) acc
    else {
      val head = str.head
      head match {
        case 'R' | 'B' =>
          val newStartIdx = (startIdx + endIdx) / 2 + 1
          getNumber(str.tail, newStartIdx, endIdx, endIdx)
        case 'L' | 'F' =>
          val newEndIdx = (startIdx + endIdx) / 2
          getNumber(str.tail, startIdx, newEndIdx, newEndIdx)
        case _ => throw new IllegalArgumentException(s"Wrong input string: $str")
      }
    }
  }

  private def getSeats(inputStr: List[String], multiplier: Int = 8): Seq[Int] = {
    inputStr
      .map(str => (str.substring(0, 7), str.substring(7, str.length)))
      .map {
        case (row, column) =>
          (getNumber(row, 0, 127), getNumber(column, 0, 7))
      }
      .map(t => t._1 * multiplier + t._2)
  }

}
