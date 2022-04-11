package com._2020.aop

object Day3 {

  val inputTestValues: String =
    """
      |..##.......
      |#...#...#..
      |.#....#..#.
      |..#.#...#.#
      |.#...##..#.
      |..#.##.....
      |.#.#.#....#
      |.#........#
      |#.##...#...
      |#...##....#
      |.#..#...#.#
      |""".stripMargin

  def solution(dataGrid: Array[Array[String]], slope: Int, down: Int): Long = {
    val listOfIndexes = dataGrid.indices
      .filter(_ % down == 0)
      .zipWithIndex
      .map(t => (t._1, t._2 * slope))
      .toList
    //    println(listOfIndexes)

    listOfIndexes.count {
      case (dx, dy) => dataGrid(dx)(dy % dataGrid.head.length) == "#"
    }
  }


  private def getResultForMultipleSteps(lines: Array[String], inputSteps: List[(Int, Int)]): Long = {
    val dataGrid = lines
      .filter(_.nonEmpty)
      .map(_.split(""))

    inputSteps.map(t => solution(dataGrid = dataGrid, slope = t._1, down = t._2)).product
  }

  def main(args: Array[String]): Unit = {
    val lines: Array[String] = inputTestValues.split("\n")
    val slopes = List((3, 1), (1, 1), (5, 1), (7, 1), (1, 2))
    println(s"Test input data solution is: ${getResultForMultipleSteps(lines, slopes)}")

    val inputFile = "src/main/resources/2020/day3.txt"
    println(s"[Part1] Solution is: ${getResultForMultipleSteps(readInputFileAsArray(inputFile), List(slopes.head))}")
    println(s"[Part2] Solution is: ${getResultForMultipleSteps(readInputFileAsArray(inputFile), slopes)}")
  }

}
