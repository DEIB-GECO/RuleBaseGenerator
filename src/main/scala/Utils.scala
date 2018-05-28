import java.io._
import java.util

import Cleaner._
import dk.brics.automaton._


object Utils {

  /* def createAllFiles(): Unit={
     new File(output_directory_path + all_keys_file)
     new File(output_directory_path + seen_keys_file)
     new File(output_directory_path + unseen_keys_file)
     new File(output_directory_path + rules_file)
   }*/

  def getListOfFiles(dir: File): Array[File] = {
    val filesList = dir.listFiles
    val res = filesList ++ filesList.filter(_.isDirectory).flatMap(getListOfFiles)
    res.filter(_.getName.endsWith(".meta"))
  }


  def buildRulePair(input_string: String): (String, String) = {
    val rule_pattern = "(.*)=>(.*)"
    (input_string.replaceAll(rule_pattern, "$1"), input_string.replaceAll(rule_pattern, "$2"))
  }


  def extractPair(input_line: String): (String, String) = {
    val key_pattern = "(.*)(\\t)(.*)"
    val key = input_line.replaceAll(key_pattern, "$1")
    val value = input_line.replaceAll(key_pattern, "$3")
    (key, value)
  }


  def extractKey(input_line: String): String = {
    val key_pattern = "(.*)(\\t)(.*)"
    val key = input_line.replaceAll(key_pattern, "$1")
    key
  }


  def rebuildLine(input_pair: (String, String)): String = {
    val line = input_pair._1 + "\t" + input_pair._2
    line
  }


  def intersection(a1: Automaton, a2: Automaton): Automaton = {
    if (a1.isSingleton) if (a2.run(a1.singleton)) return a1.cloneIfRequired
    else return BasicAutomata.makeEmpty
    if (a2.isSingleton) if (a1.run(a2.singleton)) return a2.cloneIfRequired
    else return BasicAutomata.makeEmpty
    if (a1 eq a2) return a1.cloneIfRequired
    val transitions1: Array[Array[Transition]] = Automaton.getSortedTransitions(a1.getStates)
    val transitions2: Array[Array[Transition]] = Automaton.getSortedTransitions(a2.getStates)
    val c: Automaton = new Automaton
    val worklist: util.LinkedList[StatePair] = new util.LinkedList[StatePair]
    val newstates: util.HashMap[StatePair, StatePair] = new util.HashMap[StatePair, StatePair]
    var p: StatePair = new StatePair(c.initial, a1.initial, a2.initial)
    worklist.add(p)
    newstates.put(p, p)
    while ( {
      worklist.size > 0
    }) {
      p = worklist.removeFirst
      p.s.accept = p.s1.accept && p.s2.accept
      val t1: Array[Transition] = transitions1(p.s1.number)
      val t2: Array[Transition] = transitions2(p.s2.number)
      var n1: Int = 0
      var b2: Int = 0
      while ( {
        n1 < t1.length
      }) {
        while ( {
          b2 < t2.length && t2(b2).max < t1(n1).min
        }) {
          b2 += 1;
          b2 - 1
        }
        var n2: Int = b2
        while ( {
          n2 < t2.length && t1(n1).max >= t2(n2).min
        }) {
          if (t2(n2).max >= t1(n1).min) {
            val q: StatePair = new StatePair(t1(n1).to, t2(n2).to)
            var r: StatePair = newstates.get(q)
            if (r == null) {
              q.s = new State
              worklist.add(q)
              newstates.put(q, q)
              r = q
            }
            val min: Char = if (t1(n1).min > t2(n2).min) t1(n1).min
            else t2(n2).min
            val max: Char = if (t1(n1).max < t2(n2).max) t1(n1).max
            else t2(n2).max
            p.s.transitions.add(new Transition(min, max, r.s))
          }
          {
            n2 += 1;
            n2 - 1
          }
        }

        {
          n1 += 1;
          n1 - 1
        }
      }
    }
    c.deterministic = a1.deterministic && a2.deterministic
    c.removeDeadTransitions()
    c.checkMinimizeAlways()
    c
  }


}

