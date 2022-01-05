package com.aop.aop

import scala.annotation.tailrec

object Day3 {

  def main(args: Array[String]): Unit = {
    val text = readInputFileAsString("src/main/resources/2021/day3-sample.txt")
    println(part1(text))
    println(part2(text))
  }

  def part1(text: Seq[String]): Int = {
    val data = text.map(_.toList).transpose
    val result = data.map(
      _.groupBy(identity).maxBy(_._2.size)._1
    )
    println(result)

    val gamaValues: Seq[Int] = calculateValues(result.reverse.zipWithIndex, i => i == '0')
    val epsilonValues: Seq[Int] = calculateValues(result.reverse.zipWithIndex, i => i == '1')

    val gamaRate = gamaValues.sum
    val epsilonRate = epsilonValues.sum
    gamaRate * epsilonRate
  }

  def calculateValues(indexedResult: Seq[(Char, Int)], condition: Char => Boolean): Seq[Int] = {
    val gamaValues: Seq[Int] = for {
      (i, index) <- indexedResult
      if condition(i)
    } yield Math.pow(2, index).toInt
    gamaValues
  }

  def part2(text: Seq[String]): Int = {

    def lifeSupport(binaryNums: Seq[String]): Int = {
      @tailrec
      def dfs(nums: Seq[String], idx: Int, bitPredicateFn: Seq[Char] => Int): String = {
        val remaining = nums.filter(n => n(idx) - '0' == bitPredicateFn(nums.map(_.charAt(idx))))
        if (remaining.length == 1) remaining.head else dfs(remaining, idx + 1, bitPredicateFn)
      }

      Integer.parseInt(dfs(binaryNums, idx = 0, bitsByFrequency(_)._1), 2) *
        Integer.parseInt(dfs(binaryNums, idx = 0, bitsByFrequency(_)._2), 2)
    }

    def bitsByFrequency(bits: Seq[Char]): (Int, Int) =
      Option.when(bits.count(_ == '1') >= bits.length / 2.0)((1, 0)).getOrElse((0, 1))

    lifeSupport(text)
  }
}
