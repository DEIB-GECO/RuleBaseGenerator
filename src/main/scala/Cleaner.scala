import java.io._

import scala.collection.mutable
import scala.collection.mutable._
import scala.io.Source
import scala.io.StdIn._



object Cleaner{

  val input_directory_path = "/Users/abernasconi/Documents/gitProjects/importer_data"
  val output_directory_path = "/Users/abernasconi/Documents/gitProjects/importer_data/cleaned_files/"
  val rules_file_name = "rules_base.txt"
  val keys_file_name = "keys_base.txt"




  def main(args: Array[String]): Unit = {

    KnowledgeBase.createKB;






  }




  def getRuleOrQuitChoice: String = {
    val line = readLine()
    line match {
      case "r" | "R" => println("Insert new rule to clean keys:"); line
      case "q" | "Q" => println("Cleaner is quitting... Goodbye!"); line
      case _ => println("Error, your choice is not valid. Please insert new rule to clean keys: "); getRuleOrQuitChoice
    }
  }


  def getRejectOrAcceptChoice: String = {
    val line = readLine()
    line match {
      case "n" | "N" => print("You chose to reject the specified rule!"); line
      case "y" | "Y" => println("You accepted the rule! Find the changes in \"keys_base.txt\" and \"rules_base.txt\""); line
      case _ => println("Error, your choice is not valid."); getRejectOrAcceptChoice
    }
  }





  def getListOfFiles(dir: File): Array[File] = {
    val filesList= dir.listFiles
    val res = filesList ++ filesList.filter(_.isDirectory).flatMap(getListOfFiles)
    res.filter(_.getName.endsWith(".meta"))
  }


  def buildRulePair(input_string: String): (String,String) = {
    val rule_pattern = "(.*)(.*)"
    (input_string.replaceAll(rule_pattern,"$1"), input_string.replaceAll(rule_pattern,"$3"))
  }


  def buildPair(input_line: String): (String,String) = {
    val key_pattern = "(.*)(\\t)(.*)"
    val key = input_line.replaceAll(key_pattern, "$1")
    val value = input_line.replaceAll(key_pattern, "$3")
    (key,value)
  }


  def rebuildLine(input_pair: (String,String)): String = {
    val line = input_pair._1 + "\t" + input_pair._2
   line
  }


}

