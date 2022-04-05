package com.aop.aop._2021.aop

object Day2 {

  def main(args: Array[String]): Unit = {
    val txt: Iterator[(String, Int)] = readInputFileAsTuple("src/main/resources/2021/day2.txt", " ")
    println(calculatePosition(txt.toSeq))
    println(calculatePositionUsingAim(txt.toSeq))
  }

  def calculatePosition(value: Seq[(String, Int)]): Int = {
    val (horizontal, depth) = value.foldLeft((0, 0)) {
      case ((horizontal, depth), (action, units)) =>
        action match {
          case "forward" => (horizontal + units) -> depth
          case "down" => horizontal -> (depth + units)
          case "up" => horizontal -> (depth - units)
        }
    }

    horizontal * depth
  }

  def calculatePositionUsingAim(values: Seq[(String, Int)]): Long = {
    val (horizontalPosition, depth, _) = values.foldLeft((0, 0, 0)) {
      case ((horizontal, depth, aim), (action, units)) =>
        action match {
          case "down" => (horizontal, depth, aim + units)
          case "up" => (horizontal, depth, aim - units)
          case "forward" => (horizontal + units, depth + (units * aim), aim)
        }
    }
    horizontalPosition * depth
  }

}
