package com._2020

import java.io.File
import scala.io.Source

package object aop {

  def readInputString(input: String): List[Int] =
    input
      .split("\n")
      .filter(_.nonEmpty)
      .map(_.toInt)
      .toList

  def readInputFileAsInt(inputFile: String): List[Int] = {
    readInputFile(inputFile).map(_.toInt).toList
  }

  private def readInputFile(inputFile: String): Iterator[String] = {
    Source.fromFile(new File(inputFile)).getLines()
  }
}
