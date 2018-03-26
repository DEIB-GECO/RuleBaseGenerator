import KnowledgeBase.rulesList
import scala.util.matching.Regex

case class Rule(antecedent: String, consequent: String, ordered_id: Int, rule_type: RuleType.Value) {
 // val numPattern = "[0-9]+".r
  val pattern = antecedent.r

  override def toString: String = {
    (antecedent + " => " + consequent + "\t type: " + rule_type)
  }

}
//companion object
object Rule {

  //main costructor
  //def apply(antecedent: String, consequent: String, order: Int, tp: RuleType.Value) = new Rule(antecedent, consequent, order, tp)

  //alternative costructor
  def apply(pair: (String, String), order: Int, tp: RuleType.Value) = new Rule(pair._1, pair._2, order, tp)


  def trigger(rule: Rule,new_key: String): Boolean = {
    val answer: Boolean = rule.pattern.findFirstIn(new_key).isDefined
    answer
  }

  def firstMatchedRule(key: String): Option[Rule] = KnowledgeBase.rulesList.find((r: Rule) => trigger(r,key))


  //def firstMatchedRule(key: String): Option[Rule] = rulesList.find((rule: Rule) => rule.trigger(key))
  //filter((rule: Rule) => rule.trigger(key)).headOption

  /*  {
      val answer = false
      var foundRule: Option[Rule] = None
       val key = pair._1
        val new_answer : Boolean = rule.trigger(key)
        if(!answer && new_answer){ //else if(answer) foundRule keeps old value
          foundRule = Some(rule)
        }
      }
      foundRule*
    }*/


  def simulateRule(key: String, r: Rule): Option[String] = {

    if (key.matches(r.pattern.toString)){  //if(key.matches(r.antecedent)) {
      if (r.consequent.equals("DELETE"))
        Some(key.replaceAll(r.antecedent, "NULL"))
      else
        Some(key.replaceAll(r.antecedent, r.consequent))
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


  def checkRuleInsertionOrder(a: String, t: RuleType.Value): Int = {


    0
  }


}




