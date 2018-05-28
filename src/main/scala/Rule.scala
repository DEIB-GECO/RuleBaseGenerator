import IOManager.keepNewRuleChoice
import dk.brics.automaton.{BasicOperations, RegExp, State}
import java.util.Set

import scala.collection.mutable.MutableList
import scala.util.control.Breaks.{break, breakable}

case class Rule(antecedent: String, consequent: String) extends PartiallyOrdered[Rule] {
  // val numPattern = "[0-9]+".r
  val pattern = antecedent.r

  override def toString: String = {
    (antecedent + "=>" + consequent)
  }


 override def tryCompareTo[B >: Rule](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
    if (that.isInstanceOf[Rule]) {
      val thatIns = that.asInstanceOf[Rule]
      if (this.antecedent == thatIns.antecedent) Some(0) //expressions are equivalent (or identical)
      else if (this.antecedent < thatIns.antecedent) Some(-1) //first is contained in second
      else if (this.antecedent > thatIns.antecedent) Some(1) //first contains second
      else None //not comparable
    }
    else None
  }

  /*override def tryCompareTo[B >: Rule](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
    if (that.isInstanceOf[Rule]) {
      val thatIns = that.asInstanceOf[Rule]

      var autoThis = (new RegExp (this.antecedent)).toAutomaton()
      var autoThat = (new RegExp (thatIns.antecedent)).toAutomaton()
      if(!autoThis.isDeterministic){autoThis.determinize()}
      if(!autoThat.isDeterministic){autoThat.determinize()}
      val inter = BasicOperations.intersection(autoThis,autoThat)
      val reachable: Set[State] = inter.getStates
      for(state <- reachable){

      }


      if (this.antecedent == thatIns.antecedent) Some(0) //expressions are equivalent (or identical)
      else if (this.antecedent < thatIns.antecedent) Some(-1) //first is contained in second
      else if (this.antecedent > thatIns.antecedent) Some(1) //first contains second
      else None //not comparable
    }
    else None
  }*/
}

//companion object
object Rule {

  //main costructor
  //def apply(antecedent: String, consequent: String, order: Int) = new Rule(antecedent, consequent, order)

  //alternative costructor
  def apply(pair: (String, String), order: Int) = new Rule(pair._1, pair._2)

  def StringToRule(s: String): Rule = {
    val rule_pattern = "(.*)=>(.*)"
    val r = new Rule(s.replaceFirst(rule_pattern, "$1"),s.replaceFirst(rule_pattern, "$2"))
    r
  }


  def trigger(rule: Rule, new_key: String): Boolean = {
    val answer: Boolean = rule.pattern.findFirstIn(new_key).isDefined
    answer
  }

  // def firstMatchedRule(key: String): Option[Rule] = KnowledgeBase.rulesList.find((r: Rule) => trigger(r, key))


  //def firstMatchedRule(key: String): Option[Rule] = rulesList.find((rule: Rule) => rule.trigger(key))
  //filter((rule: Rule) => rule.trigger(key)).headOption


  def simulateRule(key: String, r: Rule): Option[String] = {

    if (key.matches(r.antecedent)) {
      try {
        if (r.consequent.equals("DELETE"))
          Some(key.replaceAll(r.antecedent, "NULL"))
        else
          Some(key.replaceAll(r.antecedent, r.consequent))
      }
      catch {
        case e: Exception => println("Rule has wrong syntax!"); throw e
      }
    }
    else
      None
  }

  def applyRule(key: String, r: Rule): Option[String] = {

    if (key.matches(r.antecedent)) {
      if (r.consequent.equals("DELETE"))
        None
      else
        Some(key.replaceAll(r.antecedent, r.consequent))
    }
    else
      Some(key)
  }

  /* def applyRules(dir: String): Unit = {
     val input_files = Utils.getListOfFiles(new File(dir))
     val output_file_lines = new LinkedHashSet[String]()

     try {
       for (current_file <- input_files) {
         val bufferedSource = Source.fromFile(current_file.getAbsolutePath)
         for (line <- bufferedSource.getLines.toList) {

           val (key, value): (String, String) = Utils.extractPair(line)

           //TODO handle rules application in order of mutableList
           //  output_file_lines += (Rule.applyRule(key).get,value)
           output_file_lines += key.replaceAll("__[0-9]*__", "__X__")

         }
         bufferedSource.close
       }
     } catch {
       case e: FileNotFoundException => println("Couldn't find that file.")
       case e: IOException => println("Got an IOException!")
     }

   }*/


  def addRule(newRule: Rule, ruleList: List[Rule]): List[Rule] = {


    for (rule <- ruleList) {
      if (rule.tryCompareTo(newRule).isDefined) { //new rule is comparable with current in ruleList
        if (rule.tryCompareTo(newRule).get == 0) { //new rule is equivalent or identical to already existing rule
          if (keepNewRuleChoice(rule, newRule)) {
            return ruleList.updated(ruleList.indexOf(rule), newRule) //replace old with new rule
          }
          else
            return ruleList //return same list
        }
        else if (rule > newRule) { //the right position was passed; insert here
          val temp = ruleList.splitAt(ruleList.indexOf(rule))
          return temp._1 ::: List(newRule) ::: temp._2 //https://stackoverflow.com/questions/12600863/scala-convert-list-of-lists-into-a-single-list-listlista-to-lista
        }
        // if(rule.tryCompareTo(newRule) == -1){   }
        //} else { //new rule is comparable with current or is less than
      }
    }
    ruleList ::: List(newRule)
  }


}




