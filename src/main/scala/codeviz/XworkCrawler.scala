package codeviz

import scala.xml._ 
import com.agical.graph._


class XworkCrawler(var graph : Graph[CodeNode]) {

  val dotAction = ".action"
  
  def this() = this(new Graph[CodeNode]())
  
  def parseDocument(webappName: String, feed: Elem) : Graph[CodeNode] = {
    val packages = feed\"package"
    packages.foreach((pack) => {
      val actions = pack\"action"
      actions.foreach((action) => {
        val name = action\"@name"
        val className = action\"@class"
        val method = action\"@method"
        val classAndMethod = new MethodName(className.toString + "_" + (if (method.toString == "") "execute" else method.toString))
        connect(toActionName(webappName, name), classAndMethod, ActionToClass(_,_))
        val results = action\"result"
        results.foreach(parseActionResult(webappName, classAndMethod))
      })
    })
    graph
  }
  
  private def resultNode(text: String) = {
     val jsp = java.util.regex.Pattern.compile("(.*\\.jsp).*")
     val m = jsp.matcher(text)
     if (m.matches()) {
    	 val matched = m.group(1)
    	 Jsp(matched)
     } else {
       val action = java.util.regex.Pattern.compile("(.*\\.action).*")
       val m2 = action.matcher(text) 
       if (m2.matches()) {
      	 val matched = m2.group(1)
    	 ActionName(matched)
       } else {
         if (text == "/reportServlet2") ServletName(text)
         else if (text.startsWith("pfController")) ServletName(text)
         else if (text.startsWith("todo")) ServletName(text)
         else throw new RuntimeException("unkown node type: " + text)
       }
     }
 }
  
  private def parseActionResult(webappName: String, from: CodeNode)(result: Node) : Unit = {
	  val resultType = result\"@type"
	  resultType.toString match {
	    case "dispatcher" => {
	      val text = result.text.trim
	      connect(from, resultNode(text), ActionClassToResult(_,_))
	    }
	    case "redirect" => {
	      val location = result\"param"
	      if (location.text contains dotAction) {
	        val actionName = location.text.trim.substring(1, location.text indexOf dotAction)
	        connect(from, toActionName(webappName, actionName), ActionClassToResult(_,_))
	      } else {
	        graph
	      }
	    }
	    case "chain" => {
	      val actionName = result.text.trim
	      connect(from, toActionName(webappName, actionName), ActionClassToResult(_,_))
	    }
	    case "redirectAction" => {
	      val paramSeq = result\"param"
	      paramSeq.foreach((param) => {
	        val name = param\"@name" 
	        if (name == "actionName") {
	          val actionName = param.text.trim
	          connect(from, toActionName(webappName, actionName), ActionClassToResult(_,_))
	        } 
	      })
	    }
	    case "stream" => ()
	    case _ => {
	      printf("result type %s is not handled\n", result\"@type")
	    }
	  }
  }

  private def toActionName(webappName: String, actionName: String) : CodeNode = {
    new ActionName("/" + webappName + "/" + actionName + dotAction)
  }
  private def toActionName(webappName: String, actionName: NodeSeq) : CodeNode = 
    toActionName(webappName, actionName.toString)
  
  import com.agical.graph.Edge
  private def connect(from: CodeNode, to: CodeNode, edge:(CodeNode, CodeNode) => Edge[CodeNode]) = {
    graph = graph.connect(from)(edge(_, _))(to) 
  }
}
