package com._2020.aop

object Day2 {

  val inputTest: String =
    """
      |1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc
      |""".stripMargin

  case class Row(lowerBound: Int, upperBound: Int, char: Char, password: String)

  def solution(rows: List[String], passwordValidator: Row => Int): Int = {
    rows
      .filter(_.nonEmpty)
      .map(rowParser)
      .foldLeft(0) { (count, row) =>
        count + passwordValidator(row)
      }
  }

  private val part1PasswordValidation: Row => Int = { row =>
    if (row.lowerBound <= getTheNumberOfChar(row.password, row.char)
      && getTheNumberOfChar(row.password, row.char) <= row.upperBound) 1
    else 0
  }

  private val part2PasswordValidation: Row => Int = { row =>
    val char = row.char
    val password = row.password
    if (password(row.lowerBound - 1) == char && password(row.upperBound - 1) != char) 1
    else if (password(row.lowerBound - 1) != char && password(row.upperBound - 1) == char) 1
    else 0
  }

  private def getTheNumberOfChar(str: String, c: Char): Int = {
    val someValue = str
      .foldLeft(Map.empty[Char, Int]) { (m, c) =>
        m + (c -> (m.getOrElse(c, 0) + 1))
      }
      .get(c)

    someValue match {
      case Some(value) => value
      case None => 0
    }
  }

  private def rowParser(line: String): Row = {
    val words = line.split(" ")
    Row(getIndex(words(0))(0), getIndex(words(0))(1), extractChar(words(1)), words(2))
  }

  private def extractChar(str: String): Char = str.split(":").head(0)

  private def getIndex(str: String): Array[Int] = str.split("-").map(_.toInt)

  def main(args: Array[String]): Unit = {
    val listAsString = inputTest.split("\n").toList
    println(s"[Part1] The number of valid passwords for the test input data is: ${solution(listAsString, part1PasswordValidation)}")
    println(s"[Part2] The number of valid passwords for the test input data is: ${solution(listAsString, part2PasswordValidation)}")
    println(s"[Part1]The number of valid passwords for the test input data is: ${solution(readInputFileAsString("src/main/resources/2020/day2.txt"), part1PasswordValidation)}")
    println(s"[Part2]The number of valid passwords for the test input data is: ${solution(readInputFileAsString("src/main/resources/2020/day2.txt"), part2PasswordValidation)}")
  }

}
