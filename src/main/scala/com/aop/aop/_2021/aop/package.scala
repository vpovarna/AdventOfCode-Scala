package com.aop.aop._2021

import java.io.File
import scala.io.Source

package object aop {
  def readInputFileAsInt(filePath: String): Seq[Int] = {
    readFile(filePath).map(_.toInt).toSeq
  }

  def readInputFileAsString(filePath: String): Seq[String] = {
    readFile(filePath).toSeq
  }

  def readInputFileAsTuple(filePath: String, delimiter: String): Iterator[(String, Int)] = {
    readFile(filePath)
      .map(_.split(delimiter))
      .filter(arr => arr.length == 2)
      .map((arr: Array[String]) => (arr(0), arr(1).toInt))
  }

  private def readFile(filePath: String): Iterator[String] = {
    def open(path: String) = new File(path)

    implicit class RichFile(file: File) {
      def read: Iterator[String] = Source.fromFile(file).getLines()
    }
    open(filePath).read
  }
}
