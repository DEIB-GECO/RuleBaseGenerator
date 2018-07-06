package it.polimi.rulebasegen

import java.io.File

object Utils {

  def getListOfFiles(dir: File): Array[File] = {
    val filesList = dir.listFiles
    val res = filesList ++ filesList.filter(_.isDirectory).flatMap(getListOfFiles)
    res.filter(_.getName.endsWith(".meta"))
  }

  def buildRulePair(input_string: String): (String, String) = {
    val rule_pattern = "(.*)=>(.*)"
    (input_string.replaceAll(rule_pattern, "$1"), input_string.replaceAll(rule_pattern, "$2"))
  }

  def extractPair(input_line: String): (String, String) = {
    val key_pattern = "(.*)(\\t)(.*)"
    val key = input_line.replaceAll(key_pattern, "$1")
    val value = input_line.replaceAll(key_pattern, "$3")
    (key, value)
  }

  def extractKey(input_line: String): String = {
    val key_pattern = "(.*)(\\t)(.*)"
    val key = input_line.replaceAll(key_pattern, "$1")
    key
  }

  def rebuildLine(input_pair: (String, String)): String = {
    val line = input_pair._1 + "\t" + input_pair._2
    line
  }

}
