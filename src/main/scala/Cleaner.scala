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

object Cleaner{

  val input_directory_path = "/Users/abernasconi/Documents/gitProjects/importer_data"
  val output_directory_path = "/Users/abernasconi/Documents/gitProjects/importer_data/cleaned_files/"
  val rules_file = "rules.txt"
  val all_keys_file = "all_keys.txt"
  val unseen_keys_file = "unseen_keys.txt"
  val seen_keys_file = "seen_keys.txt"




  def main(args: Array[String]): Unit = {


  /*  var rulesList2 = readRules(rules_file)
    val rule2 =  new Rule("a","X",1)
    rulesList2 += rule2
    println(rulesList2);

    rulesList2 = rulesList2.filter(_!=rule2)
    println(rulesList2);*/


    val all_keys = computeAllKeys(input_directory_path);
    writeKeys(all_keys_file,all_keys)
    val seen_keys = readKeys(seen_keys_file)
    val rulesList = readRules(rules_file)

    KnowledgeBase.createKB(rulesList,all_keys,seen_keys);


   // KnowledgeBase.applyRules;

  }









}

