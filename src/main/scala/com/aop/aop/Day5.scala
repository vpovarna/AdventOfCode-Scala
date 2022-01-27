package com.aop.aop

import scala.annotation.tailrec

object Day5 {
  private val dataGridSize = 1000

  def main(args: Array[String]): Unit = {
    val inputData: Seq[String] = readInputFileAsString("src/main/resources/2021/day5.txt")
    val parsedData: Seq[Array[Int]] = inputParser(inputData)
    val emptyDataGrid: Array[Array[Int]] = createEmptyDataGrid(dataGridSize)
    val result = solution1TailRec(parsedData, emptyDataGrid)
    println(s"Solution1 result is: ${result.map(_.count(_ > 1)).sum}")
  }


  @tailrec
  def solution1TailRec(parsedData: Seq[Array[Int]], dataGrid: Array[Array[Int]]): Array[Array[Int]] = {
    if (parsedData.isEmpty) dataGrid
    else {
      val workingArray = parsedData.head
      val updatedDataGrid: Array[Array[Int]] = updateDataGrid(workingArray, dataGrid)
      solution1TailRec(parsedData.tail, updatedDataGrid)
    }
  }

  def updateDataGrid(workingArray: Array[Int], dataGrid: Array[Array[Int]]): Array[Array[Int]] = {
//    println(s"Array: ${workingArray.mkString("(", ", ", ")")}")
    if (workingArray(0) == workingArray(2)) {
      val startIndex = math.min(workingArray(1), workingArray(3))
      val endIndex = math.max(workingArray(1), workingArray(3))
      //    println(s"Starting index: $startIndex and end index: $endIndex")
      (startIndex to endIndex).foreach(i => {
        val t = dataGrid(i)(workingArray(0))
        if (t == 0) dataGrid(i)(workingArray(0)) = 1
        else dataGrid(i)(workingArray(0)) = t + 1
      })
    } else {
      val startIndex = math.min(workingArray(0), workingArray(2))
      val endIndex = math.max(workingArray(0), workingArray(2))
      //    println(s"Starting index: $startIndex and end index: $endIndex")
      (startIndex to endIndex).foreach(i => {
        val t = dataGrid(workingArray(1))(i)
        if (t == 0) dataGrid(workingArray(1))(i) = 1
        else dataGrid(workingArray(1))(i) = t + 1
      })
    }

//    printDataGrid(dataGrid)

    dataGrid

  }


  def inputParser(inputData: Seq[String]): Seq[Array[Int]] = {
    inputData
      .map(_.split("->").map(_.trim))
      .map(validatePairs)
      .filter(_.length == 4)
      .filter(arr => (arr(0) == arr(2)) || (arr(1) == arr(3)))
  }


  def validatePairs(pairs: Array[String]): Array[Int] = {
    pairs.flatMap(_.split(",")).map(_.toInt)
  }


  def createEmptyDataGrid(n: Int): Array[Array[Int]] = {
    Array.tabulate(n)(_ => new Array[Int](n))
  }

  def printDataGrid(dataGrid: Array[Array[Int]]): Unit = {
    dataGrid.foreach(x => println(x.mkString("(", ", ", ")")))
  }

}
