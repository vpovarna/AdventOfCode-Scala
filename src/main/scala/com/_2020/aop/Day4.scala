package com._2020.aop

import scala.annotation.tailrec

object Day4 {

  val testInput: String =
    """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
      |byr:1937 iyr:2017 cid:147 hgt:183cm
      |
      |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
      |hcl:#cfa07d byr:1929
      |
      |hcl:#ae17e1 iyr:2013
      |eyr:2024
      |ecl:brn pid:760753108 byr:1931
      |hgt:179cm
      |
      |hcl:#cfa07d eyr:2025 pid:166559648
      |iyr:2011 ecl:brn hgt:59in
      |""".stripMargin

  val validPassports: String =
    """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
      |hcl:#623a2f
      |
      |eyr:2029 ecl:blu cid:129 byr:1989
      |iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
      |
      |hcl:#888785
      |hgt:164cm byr:2001 iyr:2015 cid:88
      |pid:545766238 ecl:hzl
      |eyr:2022
      |
      |iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
      |""".stripMargin

  val invalidPassports: String =
    """eyr:1972 cid:100
      |hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
      |
      |iyr:2019
      |hcl:#602927 eyr:1967 hgt:170cm
      |ecl:grn pid:012533040 byr:1946
      |
      |hcl:dab227 iyr:2012
      |ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
      |
      |hgt:59cm ecl:zzz
      |eyr:2038 hcl:74454a iyr:2023
      |pid:3556412378 byr:2007""".stripMargin

  val validFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  @tailrec
  private def groupElements(seq: Array[String], tmpMap: Map[String, String] = Map[String, String](), acc: Array[Map[String, String]] = Array[Map[String, String]]()): Array[Map[String, String]] = {
    if (seq.isEmpty) tmpMap +: acc
    else {
      val head = seq.head
      if (head.isEmpty) groupElements(seq.tail, Map[String, String](), tmpMap +: acc)
      else {
        val filteredHead = head.split(":")
        groupElements(seq.tail, tmpMap + (filteredHead(0) -> filteredHead(1)), acc)
      }
    }
  }

  def isPassportValidPart2(fields: Map[String, String]): Boolean = {

    def dateCondition(date: String, startYear: Int, endYear: Int, length: Int = 4): Boolean = {
      def checkDate(value: String): Boolean = (value.length == length) && (value.toInt <= endYear && value.toInt >= startYear)

      verifyField(date, checkDate)

    }

    def hairCondition(hcl: String): Boolean = {
      def checkHcl(value: String): Boolean = {
        if (value.length != 7) false
        else if (value.head != '#') false
        else {
          val substring = value.substring(1, value.length)
          substring.forall(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))
        }
      }

      verifyField(hcl, checkHcl)
    }

    def heightCondition(hgt: String): Boolean = {
      def checkHgt(value: String): Boolean = {
        if (value.length > 2) {
          val unit = value.substring(value.length - 2, value.length)
          val number = value.substring(0, value.length - 2).toInt
          if (unit == "in") number >= 59 && number <= 76
          else if (unit == "cm") number >= 150 && number <= 193
          else false
        }
        else false
      }

      verifyField(hgt, checkHgt)
    }

    def passwordIdCondition(pid: String): Boolean = (pid.length == 9) && pid.forall(d => d >= '0' && d <= '9')

    def verifyField(hgt: String, checkHgt: String => Boolean): Boolean = {
      fields.get(hgt).map(checkHgt) match {
        case Some(value) => value
        case None => false
      }
    }

    validFields.forall {
      case byr@"byr" => dateCondition(byr, 1920, 2002)
      case iyr@"iyr" => dateCondition(iyr, 2010, 2020)
      case eyr@"eyr" => dateCondition(eyr, 2020, 2030)
      case hgt@"hgt" => heightCondition(hgt)
      case hcl@"hcl" => hairCondition(hcl)
      case ecl@"ecl" => verifyField(ecl, ecl => Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(ecl))
      case pid@"pid" => verifyField(pid, pid => passwordIdCondition(pid))
    }
  }

  def isPassportValidPart1(passportFields: Set[String]): Boolean = validFields.subsetOf(passportFields)

  def main(args: Array[String]): Unit = {
    val passports = groupElements(testInput.split("[\n, ]"))
    val validData = groupElements(validPassports.split("[\n, ]"))
    val invalidData = groupElements(invalidPassports.split("[\n, ]"))

    println(s"Test input data solution is: ${passports.count(fields => isPassportValidPart1(fields.keySet))}")
    println(s"Test input data solution is: ${validData.count(fields => isPassportValidPart2(fields))}")
    println(s"Test input data solution is: ${invalidData.count(fields => isPassportValidPart2(fields))}")

    val day4Input: Array[String] = readInputFileAsArray("src/main/resources/2020/day4.txt")
    val day4Passports: Array[Map[String, String]] = groupElements(day4Input.flatMap(_.split("[\n, ]")))
    println(s"[Part1] Day4 input data solution is: ${day4Passports.count(fields => isPassportValidPart1(fields.keySet))}")
    println(s"[Part2] Day4 input data solution is: ${day4Passports.count(fields => isPassportValidPart2(fields))}")
  }
}
