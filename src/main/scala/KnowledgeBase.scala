import java.io._

import Cleaner._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, LinkedHashSet, AbstractIterable}
import scala.io.Source
import scala.io.StdIn._

object KnowledgeBase {

 var rulesList = new LinkedHashSet[Rule]
 var unseen_keys = new LinkedHashSet[String]
 var seen_keys = new LinkedHashSet[String]



  def createKB: Unit = {

    //create rule base file
    val rules_base_file: File = new File(output_directory_path + rules_file_name)

    unseen_keys = buildKeysBase(input_directory_path)

    updateFile(keys_file_name,unseen_keys.map(_.asInstanceOf[Any]))
    //print keys base file
    /*val keys_base_file: File = new File(output_directory_path + keys_file_name) //not seen keys
    var bw_keys = new BufferedWriter(new FileWriter(keys_base_file))
    var bw_rules = new BufferedWriter(new FileWriter(rules_base_file))
    for(s <- unseen_keys){
      bw_keys.write(s + "\n")
    }
    bw_keys.close()*/

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
      for (key <- unseen_keys) {

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

        if (getRejectOrAcceptChoice.matches("[yY]")) {

          //apply rule
          rulesList += simulated_rule //update rules base
          for (key <- temp_new_keys) {
            unseen_keys.remove(key._1)
            seen_keys += key._1 //I save the original seen key (not the changed one)
          }

          //visualize results of application
          //bw_rules = new BufferedWriter(new FileWriter(rules_base_file))
          //for(r <- rulesList)
          //  bw_rules.write(r.toString + "\n")
          //bw_rules.close()

          updateFile(rules_file_name,rulesList.map(_.asInstanceOf[Any]))

          updateFile(keys_file_name,unseen_keys.map(_.asInstanceOf[Any]))

          //bw_keys = new BufferedWriter(new FileWriter(keys_base_file))
          //for(s <- unseen_keys)
          //  bw_keys.write(s + "\n")
          //bw_keys.close()

        }else{
          println("Resetting changes...")
        }
      }
      else
        println("Proposed rule does not apply to any key.")

      print("\nPress R (rule) to insert rule, Q (quit) to quit input procedure: ")

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

  def updateFile(file_name: String, set: LinkedHashSet[Any]): Unit ={
    val base_file: File = new File(output_directory_path + file_name) //not seen keys
    val bw = new BufferedWriter(new FileWriter(base_file))
    //var bw_rules = new BufferedWriter(new FileWriter(rules_base_file))
    for(s <- set){
      bw.write(s + "\n")
    }
    bw.close()
  }



}