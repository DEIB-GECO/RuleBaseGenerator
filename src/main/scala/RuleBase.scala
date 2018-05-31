import java.io._

import scala.util.control.Breaks._
import Cleaner._
import IOManager._
import Rule._

import scala.collection.mutable.{ArrayBuffer, LinkedHashSet, ListBuffer}
import scala.io.Source

object RuleBase {

  def createRB(readRulesList: List[Rule], all_keys: LinkedHashSet[String], seen_keys: LinkedHashSet[(String, String, Rule)]): Unit = {

    val seen_ant = new LinkedHashSet[String]
    for (x <- seen_keys) {
      seen_ant.add(x._1)
    }

    // val unseen_keys: LinkedHashSet[String] = all_keys.filter(!seen_keys.contains(_))
    val unseen_keys: LinkedHashSet[String] = all_keys.filter(!seen_ant.contains(_))
    writeKeys(unseen_keys_file, unseen_keys)

    var ruleList = readRulesList
    var preRL = List[Rule]()

    printWelcomeMsg

    while (getRuleOrQuitChoice.matches("[rR]")) {

      try {

        //create rule to be simulated
        val simulated_rule = Rule(getRuleFromUser, 0)

        //save previous state of ruleList
        preRL = ruleList

        //insertion of possible rule in RB
        ruleList = Rule.addRule(simulated_rule, ruleList)

        if (!ruleList.equals(preRL)) { //this is false if inserted rule antecedent was equivalent to existing one and user chooses to keep old one

          //simulation of RB with new rule
          val temp: (ArrayBuffer[(String, String, Rule)], ArrayBuffer[(String, String, Rule)]) = simulateRB(ruleList: List[Rule], simulated_rule: Rule, all_keys: LinkedHashSet[String])
          val temp_keys_rb = temp._1
          val temp_keys_new_rule = temp._2

          /*val temp_keys_rb = new ArrayBuffer[(String, String, Rule)]
          val temp_keys_new_rule = new ArrayBuffer[(String, String, Rule)]

          //simulation of application of RB //TODO put in separate method
          for (key <- all_keys) {
            breakable {
              for (rule <- ruleList) {
                val temp_key: Option[String] = simulateRule(key, rule)
                if (temp_key.isDefined) {
                  temp_keys_rb += ((key, temp_key.get, rule))
                  if (rule == simulated_rule)
                    temp_keys_new_rule += ((key, temp_key.get, rule))
                  break
                }

              }
            }
          }*/

          //visualization of new rule application simulation
          if (temp_keys_new_rule.nonEmpty) {
            println("The proposed rule applies to the following " + temp_keys_new_rule.size + " keys: ")
            println("%70s\t%70s\t%50s\n".format("Key before","Key after","Applied rule"))
            for (t <- temp_keys_new_rule) {
              println("%70s\t%70s\t%50s\n".format(t._1,t._2,t._3))
            }
            print("\nPress y (yes) to accept rule, n (no) to reject it: ")

            if (getRejectOrAcceptChoice.matches("[yY]")) {

              for (key_story <- temp_keys_rb) {
                unseen_keys.remove(key_story._1)
                seen_keys += key_story
              }

              updateFiles(ruleList, unseen_keys, seen_keys)

            } else {
              println("Resetting possible changes...")
              ruleList = preRL
            }
          }
          else {
            println("Proposed rule does not apply to unseen keys. It will not be included in the rule list.")
            ruleList = preRL
          }
        }

      } catch {
        case e: IndexOutOfBoundsException => ruleList = preRL
        case e: scala.MatchError =>
        case e: Exception => println("General exception, check stacktrace. ")
          e.printStackTrace()
      }

    }

  }

  def simulateRB(ruleList: List[Rule], simulated_rule: Rule, all_keys: LinkedHashSet[String]): (ArrayBuffer[(String, String, Rule)], ArrayBuffer[(String, String, Rule)]) = {

    val temp_keys_rb = new ArrayBuffer[(String, String, Rule)]
    val temp_keys_new_rule = new ArrayBuffer[(String, String, Rule)]

    for (key <- all_keys) {
      breakable {
        for (rule <- ruleList) {
          val temp_key: Option[String] = simulateRule(key, rule)
          if (temp_key.isDefined) {
            temp_keys_rb += ((key, temp_key.get, rule))
            if (rule == simulated_rule)
              temp_keys_new_rule += ((key, temp_key.get, rule))
            break
          }

        }
      }
    }
    (temp_keys_rb, temp_keys_new_rule)
  }

  def applyRB(readRulesList: List[Rule], dirIn: String, dirOut: String): Unit = {

    val input_files = Utils.getListOfFiles(new File(dirIn))

    try {
      //for every file
      for (current_file <- input_files) {
        var output_file_lines = ListBuffer[String]()
        val bufferedSource = Source.fromFile(current_file.getAbsolutePath)
        //for all the lines of the file
        for (line <- bufferedSource.getLines.toList) {
          //extract pair
          var (key, value): (String, String) = Utils.extractPair(line)
          //apply first available rule to key
          for (rule <- readRulesList) {
            val temp_key: Option[String] = simulateRule(key, rule)
            if (temp_key.isDefined) {
              key = temp_key.get
            }
          }
          //write new pair on new set
          output_file_lines += s"$key\t$value"
        }
        bufferedSource.close

        val out_file: File = new File(output_directory_path + "cleaned_" + current_file.getName)
        val bw = new BufferedWriter(new FileWriter(out_file))
        for (s <- output_file_lines) {
          bw.write(s + "\n")
        }
        bw.close()

      }
    } catch {
      case e: FileNotFoundException => println("Couldn't find that file.")
      case e: IOException => println("Got an IOException!")
        e.printStackTrace()
    }

  }


}


//https://stackoverflow.com/questions/39453125/inserting-value-into-mutablelist