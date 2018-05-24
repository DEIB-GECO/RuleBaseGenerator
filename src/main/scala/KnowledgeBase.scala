import java.io._

import scala.util.control.Breaks._

import Cleaner._
import IOManager._
import Rule._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, LinkedHashSet, MutableList}
import scala.io.Source
import scala.io.StdIn._

object KnowledgeBase {


  var unseen_keys = new LinkedHashSet[String]
  var rulesList = new MutableList[Rule]


  def createKB(readRulesList: MutableList[Rule], all_keys: LinkedHashSet[String], seen_keys: LinkedHashSet[String]): Unit = {

    unseen_keys = all_keys --= seen_keys
    writeKeys(unseen_keys_file, unseen_keys)

    rulesList = readRulesList

    while (getRuleOrQuitChoice.matches("[rR]")) {

      //read user input rule TODO move to method
      val ExpectedPatternRule = "(.*)\\s*(=>)\\s*(.*)".r
      val ExpectedPatternRule(a, b, c) = readLine()

      //automatically decide rule order
      val rule_order: Int = checkRuleInsertionOrder(a, rulesList)

      //create rule to be simulated
      val simulated_rule = Rule(a, c, rule_order)

      val temp_new_keys = new ArrayBuffer[(String, String)]

      //simulation of application of rule (wrt its order)

      try {
        for (key <- unseen_keys) {
          val new_key: Option[String] = simulateRule(key, simulated_rule)
          if (new_key.isDefined)
            temp_new_keys += ((key, new_key.get))
        }

        //visualization of rule application simulation
        if (temp_new_keys.nonEmpty) {
          println("Proposed rule applies to the following " + temp_new_keys.size + " keys: ")
          for (temp <- temp_new_keys) {
            println("Key before: " + temp._1 + "\tKey after: " + temp._2)
          }
          print("\nPress y (yes) to accept rule, n (no) to reject it: ")

          if (getRejectOrAcceptChoice.matches("[yY]")) {

            //accept rule (insert it in keys base)
            val temp = rulesList.splitAt(rule_order + 1)
            rulesList = temp._1 ++ MutableList(simulated_rule) ++ temp._2 //update rules base
            for (key <- temp_new_keys) {
              unseen_keys.remove(key._1)
              seen_keys += key._1 //I save the original seen key (not the changed one)
            }

            writeRules(rules_file, rulesList)
            writeKeys(unseen_keys_file, unseen_keys)
            writeKeys(seen_keys_file, seen_keys)

          } else {
            println("Resetting changes...")
          }
        }
        else
          println("Proposed rule does not apply to any key.")

      } catch {
        case e: IndexOutOfBoundsException =>
      }

    }

  }


}


//https://stackoverflow.com/questions/39453125/inserting-value-into-mutablelist