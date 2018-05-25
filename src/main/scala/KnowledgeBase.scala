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


  //var unseen_keys = new LinkedHashSet[String]
  // var ruleList =  List[Rule]()
  // var rulesList = RuleBase.ruleList


  def createKB(readRulesList: List[Rule], all_keys: LinkedHashSet[String], seen_keys: LinkedHashSet[String]): Unit = {

    val unseen_keys: LinkedHashSet[String] = all_keys.filter(!seen_keys.contains(_))
    writeKeys(unseen_keys_file, unseen_keys)

    var ruleList = readRulesList

    while (getRuleOrQuitChoice.matches("[rR]")) {

      //create rule to be simulated
      try {

        val simulated_rule = Rule(getRuleFromUser, 0)

        //save previous state of ruleList
        val preRL = ruleList

        //insertion of possible rule
        ruleList = ruleList :+ simulated_rule //TODO check if insertion was in order

        val temp_new_keys = new ArrayBuffer[(String, String)]

        //simulation of application of RB
        for (key <- all_keys) {
          breakable {
            for (rule <- ruleList) {
              val new_key: Option[String] = simulateRule(key, rule)
              if (new_key.isDefined) {
                temp_new_keys += ((key, new_key.get))
                break
              }
            }
          }
        }

        //visualization of rule application simulation
        if (temp_new_keys.nonEmpty) {
          val temp_new_keys_matched: ArrayBuffer[(String, String)] = temp_new_keys.filter(_._1.matches(simulated_rule.antecedent))
          if (temp_new_keys_matched.nonEmpty) {
            println("The proposed rule applies to the following " + temp_new_keys_matched.size + " keys: ")
          } else {
            println("No key affected by proposed rule")
          }
          for (temp <- temp_new_keys_matched) {
            println("Key before: " + temp._1 + "\tKey after: " + temp._2)
          }
          print("\nPress y (yes) to accept rule, n (no) to reject it: ")

          if (getRejectOrAcceptChoice.matches("[yY]")) {

            for (key <- temp_new_keys) {
              unseen_keys.remove(key._1)
              seen_keys += key._1 //I save the original seen key (not the changed one)
            }

            writeRules(rules_file, ruleList)
            writeKeys(unseen_keys_file, unseen_keys)
            writeKeys(seen_keys_file, seen_keys)

          } else {
            println("Resetting possible changes...")
            ruleList = preRL
          }
        }
        else
          println("Proposed rule does not apply to any key.")

      } catch {
        case e: IndexOutOfBoundsException => ;
        case e: Exception => println("beccato caso di inserimento sbagliato regola")


      }

    }

  }


}


//https://stackoverflow.com/questions/39453125/inserting-value-into-mutablelist