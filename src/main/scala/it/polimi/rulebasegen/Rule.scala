package it.polimi.rulebasegen

import dk.brics.automaton._
import it.polimi.rulebasegen.IOManager.keepNewRuleChoice


case class Rule(antecedent: String, consequent: String) extends PartiallyOrdered[Rule] {
  //val pattern = antecedent.r

  override def toString: String = {
    antecedent + "=>" + consequent
  }

  /* override def tryCompareTo[B >: it.polimi.rulebasegen.Rule](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
     if (that.isInstanceOf[it.polimi.rulebasegen.Rule]) {
       val thatIns = that.asInstanceOf[it.polimi.rulebasegen.Rule]

       val autoThis = (new RegExp(this.antecedent)).toAutomaton()
       autoThis.expandSingleton()
       autoThis.determinize()

       val autoThat = (new RegExp(thatIns.antecedent)).toAutomaton()
       autoThat.expandSingleton()
       autoThat.determinize()

       if (autoThis.equals(autoThat)) {
         return Some(0)
       }
       val inter = BasicOperations.intersection(autoThis, autoThat)
       val reachable: Set[State] = inter.getStates.asScala
       if (reachable.forall(x => x.getAcceptreject != AcceptRejectCondition.BOTH)) None
       else if (reachable.forall(x => x.getAcceptreject != AcceptRejectCondition.LEFT)) Some(-1)
       else if (reachable.forall(x => x.getAcceptreject != AcceptRejectCondition.RIGHT)) Some(1)
       else None
     }
     else None
   }*/

  override def tryCompareTo[B >: Rule](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
    if (that.isInstanceOf[Rule]) {
      val thatIns = that.asInstanceOf[Rule]

      val autoThis = new RegExp(this.antecedent).toAutomaton()
      autoThis.expandSingleton()
      autoThis.determinize()

      val autoThat = new RegExp(thatIns.antecedent).toAutomaton()
      autoThat.expandSingleton()
      autoThat.determinize()

      if (autoThis.equals(autoThat)) Some(0)
      else {
        val inter = BasicOperations.intersection(autoThis, autoThat)
        inter.determinize()
        if (inter.equals(autoThat)) Some(1)
        else if (inter.equals(autoThis)) Some(-1)
        else None
      }
    } else None

  }

}

//companion object
object Rule {

  //costructor
  def apply(pair: (String, String), order: Int) = new Rule(pair._1, pair._2)

  def StringToRule(s: String): Rule = {
    val rule_pattern = "(.*)=>(.*)"
    val r = new Rule(s.replaceFirst(rule_pattern, "$1"), s.replaceFirst(rule_pattern, "$2"))
    r
  }

  // def firstMatchedRule(key: String): Option[it.polimi.rulebasegen.Rule] = KnowledgeBase.rulesList.find((r: it.polimi.rulebasegen.Rule) => trigger(r, key))


  //def firstMatchedRule(key: String): Option[it.polimi.rulebasegen.Rule] = rulesList.find((rule: it.polimi.rulebasegen.Rule) => rule.trigger(key))
  //filter((rule: it.polimi.rulebasegen.Rule) => rule.trigger(key)).headOption

  def simulateRule(key: String, r: Rule): Option[String] = {

    if (key.matches(r.antecedent)) {
      try {
        if (r.consequent.equals("NULL"))
          Some(key.replaceAll(r.antecedent, "NULL"))
        else
          Some(key.replaceAll(r.antecedent, r.consequent))
      }
      catch {
        case e: Exception => println("it.polimi.rulebasegen.Rule has wrong syntax!"); throw e
      }
    }
    else
      None
  }

  def addRule(newRule: Rule, ruleList: List[Rule]): List[Rule] = {

    for (rule <- ruleList) {
      val rel = rule.tryCompareTo(newRule)
      if (rel.isDefined) { //new rule is comparable with current in ruleList
        if (rel.get == 0) { //new rule is equivalent or identical to already existing rule
          if (newRule != rule && keepNewRuleChoice(rule, newRule)) {
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




