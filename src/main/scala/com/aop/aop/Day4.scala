package com.aop.aop

import scala.util.control.Breaks.{break, breakable}

object Day4 {


  def main(args: Array[String]): Unit = {
    val inputData = readInputFileAsString("src/main/resources/2021/day4-sample.txt")

    val generatedNumbers: String = inputData.head
    val intGenNumbers = generatedNumbers.split(",").map(_.toInt)

    val boards = readBoards(inputData.tail)
    boards.foreach(println)

    var tmpMap: Map[Int, List[Int]] = Map.empty

   var winningCombinations: List[(Int, Int)] = List.empty
     breakable {
      for (extractedNr <- intGenNumbers) {
        for (board <- boards) {
          for (row <- board) {
            if (row.contains(extractedNr)) {
              updateTmpMap(extractedNr, board)
              if (checkForBingo(board, extractedNr, board.indexOf(row), boards.indexOf(board))) {
                winningCombinations =  (extractedNr -> boards.indexOf(board)) :: winningCombinations
                break
              }
            }
          }
        }
      }
    }

    def updateTmpMap(extractedNr: Int, board: Seq[Seq[Int]]): Unit = {
      val bordIndex = boards.indexOf(board)
      if (tmpMap.contains(bordIndex)) {
        tmpMap += (bordIndex -> (extractedNr :: tmpMap(bordIndex)))
      } else {
        tmpMap += (bordIndex -> List.empty)
      }
    }

    def checkForBingo(board: Seq[Seq[Int]], extractedNr: Int, index: Int, boardIndex: Int) = {
      val row = board(index)
      val transposeBoard: Seq[Seq[Int]] = board.transpose
      val transposedRow = transposeBoard(index)
      val matchAll: Boolean = row.forall(x => tmpMap(boardIndex).contains(x)) | transposedRow.forall(x => tmpMap(boardIndex).contains(x))
      matchAll
    }

    println(tmpMap)
    val (winingNr, winingBoardIndex) = winningCombinations.head
    val winingBoard = boards(winingBoardIndex)
    winingBoard.flatten.filter(el => !tmpMap(winingBoardIndex).contains(el)).foreach(println)

  }

  def readBoards(inputBoards: Seq[String]): Seq[Seq[Seq[Int]]] = {
    val boards = inputBoards.filter(_ != "")
      .flatMap { row =>
        row.split(" ")
          .filter(_.nonEmpty)
          .map(_.toInt)
      }.sliding(5, 5)
      .grouped(5)
      .toSeq
    boards
  }

}
