import scala.util.control.Breaks._

import Cleaner._
import IOManager._
import Rule._

import scala.collection.mutable.{ArrayBuffer, LinkedHashSet}

object KnowledgeBase {

  def createKB(readRulesList: List[Rule], all_keys: LinkedHashSet[String], seen_keys: LinkedHashSet[String]): Unit = {

    val unseen_keys: LinkedHashSet[String] = all_keys.filter(!seen_keys.contains(_))
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
            for (temp <- temp_keys_new_rule) {
              println("Key before: " + temp._1 + "\tKey after: " + temp._2 + "\tApplied rule: " + temp._3)
            }
            print("\nPress y (yes) to accept rule, n (no) to reject it: ")

            if (getRejectOrAcceptChoice.matches("[yY]")) {

              for (key <- temp_keys_rb) {
                unseen_keys.remove(key._1)
                seen_keys += key._1
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


}



//https://stackoverflow.com/questions/39453125/inserting-value-into-mutablelist