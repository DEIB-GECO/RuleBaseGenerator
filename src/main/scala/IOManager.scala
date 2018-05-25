import java.io._
import java.nio.file.{Files, Paths}

import Cleaner._

import scala.collection.mutable.{LinkedHashSet, MutableList}
import scala.io.Source
import scala.io.StdIn._


object IOManager{



  def computeAllKeys(dir: String): LinkedHashSet[String] ={
    val input_files = Utils.getListOfFiles(new File(dir))
    val output_file_lines = new LinkedHashSet[String]()

    try {
      for(current_file <- input_files) {
        val bufferedSource = Source.fromFile(current_file)
        for (line <- bufferedSource.getLines.toList) {
          output_file_lines += Utils.extractKey(line).replaceAll("__[0-9]*__","__X__")
        }
        bufferedSource.close
      }
    } catch{
      case e: FileNotFoundException => println("Couldn't find that file.")
      case e: IOException => println("Got an IOException!")
    }

    output_file_lines
  }

  def writeKeys(file_name: String, set: LinkedHashSet[String]): Unit ={
    val base_file: File = new File(output_directory_path + file_name)
    val bw = new BufferedWriter(new FileWriter(base_file))
    for(s <- set){
      bw.write(s + "\n")
    }
    bw.close()
  }

  def writeRules(file_name: String, lis: List[Rule]): Unit ={
    val base_file: File = new File(output_directory_path + file_name)
    val bw = new BufferedWriter(new FileWriter(base_file))
    for(s <- lis){
      bw.write(s + "\n")
    }
    bw.close()
  }


  def readKeys(file_name: String): LinkedHashSet[String] ={
    val output_file_lines = new LinkedHashSet[String]()
    try {
      val bufferedSource = Source.fromFile(output_directory_path + file_name)
      for (line <- bufferedSource.getLines) {
        output_file_lines += line
      }
      bufferedSource.close
    } catch{
      case e: FileNotFoundException => println("Couldn't find that file.");
      case e: IOException => println("Got an IOException!")
    }
    output_file_lines
  }

  def readRules(file_name: String): List[Rule] ={
    var rulesList = List[Rule]()
    try {
      val bufferedSource = Source.fromFile(output_directory_path + file_name)
      for (line <- bufferedSource.getLines) {
        rulesList = rulesList ::: List(Rule.StringToRule(line))
      }
      bufferedSource.close
    } catch{
      case e: FileNotFoundException => println("Couldn't find file " + file_name)
      case e: IOException => println("Got an IOException!")
    }
    rulesList
  }

  def getRuleOrQuitChoice: String = {

    println("\nPlease open the \"" + unseen_keys_file + "\" and \"" + rules_file + "\" files and get inspiration for new cleaning rules!")
    print("\nPress R (rule) to insert rule, Q (quit) to quit input procedure: ")

    val line = readLine()
    line match {
      case "r" | "R" => println("Insert new rule to clean keys (with syntax antecedent=>consequent):"); line
      case "q" | "Q" => println("Cleaner is quitting... Goodbye!"); line
      case _ => println("Error, your choice is not valid. Please insert new rule to clean keys: "); getRuleOrQuitChoice
    }
  }


  def getRejectOrAcceptChoice: String = {
    val line = readLine()
    line match {
      case "n" | "N" => print("You chose to reject the specified rule! "); line
      case "y" | "Y" => println("\nYou accepted the rule! Find the changes (if any) in \"" + seen_keys_file + "\" and \"" + rules_file + "\"\n"); line
      case _ => println("Error, your choice is not valid."); getRejectOrAcceptChoice
    }
  }

  def getRuleFromUser: (String,String) = {
      try{
        val ExpectedPatternRule = "(.*)=>(.*)".r
        val ExpectedPatternRule(a, c) = readLine()
        (a,c)
      } catch {
        case e: Exception => println("Cannot read the input rule!"); throw e
      }
  }





}

