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

  private def readInputFile(inputFile: String): Iterator[String] = {
    Source.fromFile(new File(inputFile)).getLines()
  }
}
