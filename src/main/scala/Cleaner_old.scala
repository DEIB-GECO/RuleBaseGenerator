import java.io._

import scala.collection.mutable._
import scala.io.Source
import scala.io.StdIn._


object Cleaner_old{

  val input_directory_path = "/Users/abernasconi/Documents/gitProjects/importer_data"
  val output_directory_path = "/Users/abernasconi/Documents/gitProjects/importer_data/cleaned_files/"
  val rules_file_name = "rules_base.txt"
  val keys_file_name = "keys_base.txt"




  def main(args: Array[String]): Unit = {




    //create rule base file
    val rules_base_file: File = new File(output_directory_path + rules_file_name)

    KnowledgeBase.unseen_keys = buildKeysBase(input_directory_path)

    //print keys base file
    val keys_base_file: File = new File(output_directory_path + keys_file_name) //not seen keys
    var bw_keys = new BufferedWriter(new FileWriter(keys_base_file))
    var bw_rules = new BufferedWriter(new FileWriter(rules_base_file))
    for(s <- KnowledgeBase.unseen_keys){
      bw_keys.write(s + "\n")
    }
    bw_keys.close()

    println("""Please open the "keys_base.txt" and "rules_base.txt" files and get inspiration for new cleaning rules!""")

    print("\nPress R (rule) to insert rule, Q (quit) to quit input procedure: ")

    while(getRuleOrQuitChoice.matches("[rR]")){

      //read user input rule TODO move to method
      val ExpectedPatternRule = "(.*)\\s*(=>)\\s*(.*)".r
      val inputRule = readLine()
      val ExpectedPatternRule(a, b, c) = inputRule
      val rule_type = if (a.contains(".*")) RuleType.GLOBAL else RuleType.LOCAL

      //automatically decide rule order
      val rule_order: Int = Rule.checkRuleInsertionOrder(a, rule_type)

      //create rule to be simulated
      var simulated_rule = Rule(a, c, rule_order, rule_type)

      val temp_new_keys = new ArrayBuffer[(String,String)]

      //simulation of application of rule (wrt its order)
      for (key <- KnowledgeBase.unseen_keys) {

        val new_key: Option[String] = Rule.simulateRule(key, simulated_rule)
        if (new_key.isDefined)
          temp_new_keys += ((key, new_key.get))

      }

      //visualization of rule application simulation
      if (temp_new_keys.nonEmpty){
        println("Proposed rule applies to the following keys: ")
        for (temp <- temp_new_keys) {
          println("Key before: " + temp._1 + "\tKey after: " + temp._2)
        }
        print("\nPress y (yes) to accept rule, n (no) to reject it: ")

        //val rejectOrAcceptChoiceType = getRejectOrAcceptChoice
        //val ExpectedPatternAccept = "[ynYN]".r
        //val inputAccept = readLine()
        //val ExpectedPatternAccept(acceptance) = inputAccept

        if (getRejectOrAcceptChoice.matches("[yY]")) {

          //apply rule
          KnowledgeBase.rulesList += simulated_rule //update rules base
          for (key <- temp_new_keys) {
            KnowledgeBase.unseen_keys.remove(key._1)
            KnowledgeBase.seen_keys += key._1 //I save the original seen key (not the changed one)
          }

          //visualize results of application
          bw_rules = new BufferedWriter(new FileWriter(rules_base_file))
          for(r <- KnowledgeBase.rulesList)
            bw_rules.write(r.toString + "\n")
          bw_rules.close()
          bw_keys = new BufferedWriter(new FileWriter(keys_base_file))
          for(s <- KnowledgeBase.unseen_keys)
            bw_keys.write(s + "\n")
          bw_keys.close()

        }else{
          println("Resetting changes...")
        }
      }
      else
        println("Proposed rule does not apply to any key.")

      print("\nPress R (rule) to insert rule, Q (quit) to quit input procedure: ")

    }


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
      case "n" | "N" => print("You chose not to accept the specified rule! "); line
      case "y" | "Y" => println("You accepted the rule! Find the changes in \"keys_base.txt\" and \"rules_base.txt\""); line
      case _ => println("Error, your choice is not valid."); getRejectOrAcceptChoice
    }
  }


  def buildKeysBase(dir: String): LinkedHashSet[String] ={
    val input_files = getListOfFiles(new File(dir))
    val output_file_lines = new LinkedHashSet[String]()

    try {
      for(current_file <-input_files) {
        val bufferedSource = Source.fromFile(current_file.getAbsolutePath)
        for (line <- bufferedSource.getLines.toList) {
          val (key, value): (String, String) = buildPair(line)
          output_file_lines += (key)
        }
        bufferedSource.close
      }
    } catch{
      case e: FileNotFoundException => println("Couldn't find that file.")
      case e: IOException => println("Got an IOException!")
    }

    output_file_lines
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

