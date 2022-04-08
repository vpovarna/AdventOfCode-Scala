package com._2021.aop

object Day1 extends App {

  val inputValues: Seq[Int] = readInputFileAsInt("src/main/resources/2021/day1.txt")
  println(countMeasurements(inputValues, 1))
  println(countMeasurements(inputValues, 3))

  def countMeasurements(fileContent: Seq[Int], step: Int): Int = {
    (step until fileContent.length).count(i => {
      fileContent(i) - fileContent(i - step) > 0
    })
  }

}
