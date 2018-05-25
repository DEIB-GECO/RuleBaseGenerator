import java.io.{File, FileNotFoundException, IOException}

import scala.collection.mutable.{LinkedHashSet, MutableList}
import scala.io.Source
import scala.util.matching.Regex

case class Rule(antecedent: String, consequent: String, ordered_id: Int) extends PartiallyOrdered[String]{
  // val numPattern = "[0-9]+".r
  val pattern = antecedent.r

  override def toString: String = {
    (ordered_id + "\t" + antecedent + "=>" + consequent)
  }


  override def tryCompareTo[B >: String](that: B)(implicit evidence$1: B => PartiallyOrdered[B]): Option[Int] = {
    if (this.antecedent == that) Some(0)
    else if (this.antecedent < that) Some(-1)
    else if (this.antecedent > that) Some(1)
    else None
  }

}

//companion object
object Rule {

  //main costructor
  //def apply(antecedent: String, consequent: String, order: Int, tp: RuleType.Value) = new Rule(antecedent, consequent, order, tp)

  //alternative costructor
  def apply(pair: (String, String), order: Int) = new Rule(pair._1, pair._2, order)

  def StringToRule(s: String): Rule = {
    val rule_pattern = "([0-9]+)\\t(.*)=>(.*)"
    val r = new Rule(s.replaceFirst(rule_pattern, "$2"),
      s.replaceFirst(rule_pattern, "$3"),
      Integer.parseInt(s.replaceFirst(rule_pattern, "$1")))
    r
  }


  def trigger(rule: Rule, new_key: String): Boolean = {
    val answer: Boolean = rule.pattern.findFirstIn(new_key).isDefined
    answer
  }

 // def firstMatchedRule(key: String): Option[Rule] = KnowledgeBase.rulesList.find((r: Rule) => trigger(r, key))


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

  def checkRuleInsertionOrder(a: String, rulesList: MutableList[Rule]): Int = {

    for (rule <- rulesList) {
      if (a.matches(rule.antecedent))
        rule.ordered_id - 1
    }
    0
  }


}




