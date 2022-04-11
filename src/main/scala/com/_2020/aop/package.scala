package com._2020

import java.io.File
import scala.io.Source

package object aop {

  def readInputFileAsInt(inputFile: String): List[Int] = {
    readInputFile(inputFile).map(_.toInt).toList
  }

  def readInputFileAsString(inputFile: String): List[String] = {
    readInputFile(inputFile).toList
  }

  def readInputFileAsArray(inputFile: String): Array[String] = {
    readInputFile(inputFile).toArray
  }

  private def readInputFile(inputFile: String): Iterator[String] = {
    Source.fromFile(new File(inputFile)).getLines()
  }

  private def printDataGrid(dataGrid: Array[Array[String]]): Unit = {
    dataGrid.foreach(arr => println(arr.mkString("[", "", "]")))
  }

}
