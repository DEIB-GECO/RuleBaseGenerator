import java.util.Comparator
import java.util.function.Supplier

import IOManager.getRejectOrAcceptChoice
import org.jgrapht.experimental.dag.DirectedAcyclicGraph
import org.jgrapht.graph.DefaultEdge

import scala.collection.mutable


object RuleBase{
//  var ruleList = new mutable.MutableList[Rule]
 // ruleList += new Rule("a","X",1)
 // ruleList += new Rule("b","X",2)
 // ruleList += new Rule("b","C",3)




  //https://stackoverflow.com/questions/2683879/scala-2-8-treemap-and-custom-ordering
  //https://stackoverflow.com/questions/4891340/mixing-java-with-scala-in-order-to-use-a-mutable-treemap (not working)
 /* implicit object A extends Ordering[String] {
    def compare(o1: String, o2: String): Int = {
      if (o1.charAt(1)<o2.charAt(1)) -1
      else if(o1.charAt(1)==(o2.charAt(1))) 0
      else 1
    }
  }



  var rb = new mutable.TreeMap[String,Rule]()

  rb += ("AL"-> new Rule("a","X",1))

  rb += ("OH"-> new Rule("b","X",2))

  rb += ("BH"-> new Rule("b","X",2))

  println(rb.toString())*/







}

