import java.io._

import scala.collection.mutable
import scala.collection.mutable._
import scala.io.Source
import scala.io.StdIn._
import IOManager._
import dk.brics.automaton.Automaton
import java.lang.Object

import org.jgrapht.experimental.dag.DirectedAcyclicGraph
import org.jgrapht.graph

object Cleaner {

  val input_directory_path = "/Users/abernasconi/Documents/gitProjects/importer_data/input_files"
  val output_directory_path = "/Users/abernasconi/Documents/gitProjects/importer_data/cleaned_files/"
  val rules_file = "rules.txt"
  val all_keys_file = "all_keys.txt"
  val unseen_keys_file = "unseen_keys.txt"
  val seen_keys_file = "seen_keys.txt"


  def main(args: Array[String]): Unit = {

    //creation of RB
    val all_keys = computeAllKeys(input_directory_path);
    writeKeys(all_keys_file, all_keys)
    val seen_keys = readSeenKeys(seen_keys_file)
    var ruleList = readRules(rules_file)
    RuleBase.createRB(ruleList, all_keys, seen_keys);

    //application of RB
    ruleList = readRules(rules_file)
    RuleBase.applyRB(ruleList,input_directory_path,output_directory_path);

  }


}

