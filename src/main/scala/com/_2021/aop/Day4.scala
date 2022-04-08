package com._2021.aop

import scala.annotation.tailrec

object Day4 {

  def main(args: Array[String]): Unit = {
    val inputData = readInputFileAsString("src/main/resources/2021/day4.txt")

    val generatedNumbers: String = inputData.head
    val intGenNumbers = generatedNumbers.split(",").map(_.toInt).toList
    val boards = readBoards(inputData.tail.toList)

    println(s"Solution1 response: ${solution1(intGenNumbers, boards)}")
    println(s"Solution2 response: ${solution2(intGenNumbers, boards)}")
  }

  @tailrec
  def solution1(numbers: List[Int], grids: List[Board]): Int = {
    numbers match {
      case h :: t =>
        val updated = grids.map(_ + h)
        val bingo = updated.find(b => b.bingo())
        if (bingo.isDefined) {
          val winingBoard = bingo.get
          println(s"Wining nr: $h")
          val sum = winingBoard.score()
          println(s"Wining board sum is: $sum")
          sum * h
        } else {
          solution1(t, updated)
        }
      case Nil => throw new IllegalArgumentException("No bingo!")
    }

  }

  @tailrec
  def solution2(numbers: List[Int], grids: List[Board]): Int = {
    numbers match {
      case h :: t =>
        val updated = grids.map(_ + h)
        if (updated.size > 1) {
          val filtered = updated.filter(b => !b.bingo())
          solution2(t, filtered)
        } else {
          if (updated.head.bingo()) {
            updated.head.score() * h
          } else {
            solution2(t, updated)
          }
        }

      case Nil => throw new IllegalArgumentException("No bingo!")
    }

  }

  def readBoards(inputBoards: List[String]): List[Board] = {
    inputBoards.sliding(6, 6)
      .map(l => l.filter(_.nonEmpty)
        .map(_.split(" ").toList.filter(_.nonEmpty).map(_.toInt)))
      .map(new Board(_))
      .toList

  }

  class Board(private val rows: List[List[(Int, Boolean)]], private val columns: List[List[(Int, Boolean)]]) {

    def this(numbers: List[List[Int]]) {
      this(numbers.map(row => row.map((_, false))), numbers.map(row => row.map((_, false))).transpose)
    }

    def bingo(): Boolean = {
      rows.exists(_.forall(r => r._2)) | columns.exists(_.forall(c => c._2))
    }

    def score(): Int = {
      rows.flatMap(r => r.filter(!_._2).map(_._1)).sum
    }

    def +(h: Int): Board = {
      def updateNrExtractedInfo(rows: List[List[(Int, Boolean)]]): List[List[(Int, Boolean)]] = {
        rows.map { r =>
          r.map { t => {
            val value = t._1
            val status = t._2
            if (!status && value == h) value -> true
            else value -> status
          }
          }
        }
      }

      val updatedRows = updateNrExtractedInfo(rows)
      val updatedColumns = updateNrExtractedInfo(columns)
      new Board(updatedRows, updatedColumns)
    }

    override def toString: String = {
      s"Board: $rows \nTransposed Board: $columns"
    }
  }
}
