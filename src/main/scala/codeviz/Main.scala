package codeviz

object Main {

  def main(args: Array[String]): Unit = {
    doWork()
  }

  import java.io.{ FileInputStream, PrintStream, FileOutputStream }
  import com.agical.graph._
  import scala.xml.XML
  import java.io.File

  def doWork(): Unit = {
    val panRootDir = "/Users/ennorunne/workspaceJuno/Codeviz/"
    val panDir = panRootDir + "src/main/scala/"

    val scanWebApps = false

    val classDirs =
      new File(panRootDir + "target/scala-2.9.2/classes/") ::
        Nil

    //    val xworkFilenames =
    //       scanForXworkFiles(
    //    	   ("pan", panDir + "webapp/WEB-INF/classes/") ::
    //           Nil)
    //
    //    val panWebappDir = panDir + "webapp" 
    //    
    //    val jspDirectories = 
    //        ("pan", panWebappDir, "/") ::
    //        Nil

    var graph = new Graph[CodeNode]()

    //    val manualDependencies = 
    //      (MenuItem("Menu: Management"), ActionName("/ManagementRead.action")) ::
    //      (Jsp("/management.jsp"), IncludeFile("/popup.inc")) ::
    //      Nil
    //    manualDependencies.foreach((item) => {
    //      val from = item._1
    //      val to = item._2
    //      graph = connect(graph, Manual(_,_), from, to)
    //    })

    //    if (scanWebApps) {
    //      val xworkCrawler = new XworkCrawler(graph)
    //	    xworkFilenames.foreach((tuple) => {
    //	        val webappName = tuple._1
    //	        val filename = tuple._2
    //	    	val is = new FileInputStream(filename)
    //	    	val document = XML.load(is)
    //	    	graph = xworkCrawler.parseDocument(webappName, document)
    //	     })
    //	  jspDirectories.foreach((dirInfo) => {
    //	    val webappName = dirInfo._1
    //	    val directory = dirInfo._2
    //	    val subDir = dirInfo._3
    //		graph = new JspCrawler(webappName, directory, subDir).scanJsps(graph)
    //	  })
    //    }

    println("classDirs: " + classDirs.mkString)
    graph = ClassCrawler.crawl(graph, new GraphMethodUpdater(_), classDirs)

    val sources =
      //      ActionName("/pan/merchantManagementList.action") ::
      //      MenuItem("Menu: Program Management") ::
      //      ActionName("/pan/aReport3Selection.action") ::
      MethodName("codeviz.GraphUpdater_visit") ::
        Nil
    //	val filteredGraph = filterFromSources(sources, graph, followDependencyToTradeDoublerClass, (node: CodeNode) => true) 

    val targets =
      MethodName("codeviz.GraphUpdater_shouldConnect") ::
        Nil
    //	val filteredGraph = filterFromSources(sources, graph, followAllDependencies, (node: CodeNode) => true)
    val filteredGraph = filterToTargets(targets, graph, followDependencies("codeviz."))

    writeToFile(filteredGraph, new StyleSheet[CodeNode] {
      override def nodeStyle(node: CodeNode) = {
        val default = Map("label" -> nodeNameToLabel(node))
        if (sources.contains(node.name)) Some(default ++ Map("color" -> "red", "style" -> "filled"))
        else node match {
          case ActionName(_) ⇒ Some(default ++ Map("shape" -> "rect", "color" -> "green", "style" -> "filled"))
          case MenuItem(_)   ⇒ Some(default ++ Map("shape" -> "hexagon", "color" -> "yellow", "style" -> "filled"))
          case _             ⇒ Some(default)
        }
      }

      private def nodeNameToLabel = (node: CodeNode) ⇒ node match {
        case MethodName(name) ⇒ name.substring(name.lastIndexOf('.') + 1)
        case _                ⇒ node.name
      }

      override def edgeStyle(edge: Edge[CodeNode]): Option[Map[String, Any]] = {
        edge match {
          case ActionToClass(_, _)       ⇒ Some(Map("label" -> "ActionClass"))
          case ActionClassToResult(_, _) ⇒ Some(Map("label" -> "ActionResult"))
          case JspToAction(_, _)         ⇒ Some(Map("label" -> "JspToAction"))
          case JspInclude(_, _)          ⇒ Some(Map("label" -> "Include"))
          case Script(_, _)              ⇒ Some(Map("label" -> "Script"))
          case _                         ⇒ None
        }
      }
    })
  }

  private def scanForXworkFiles(rootDirList: List[(String, String)]) = {
    var xworkFilenames: List[(String, String)] = Nil
    def scanDirectoryForXworkFiles(webappName: String, dir: String) = ScanDirectory(new File(dir),
      (file) ⇒ file.getName.matches("xwork-.*\\.xml") || file.getName.matches("struts-.*\\.xml"),
      (file) ⇒ xworkFilenames = (webappName, file.getAbsolutePath) :: xworkFilenames)
    rootDirList.foreach((tuple) ⇒ scanDirectoryForXworkFiles(tuple._1, tuple._2))
    xworkFilenames
  }

  private def followAllDependencies(edge: Edge[CodeNode]) = true

  private def followDependencies(classPrefix: String)(edge: Edge[CodeNode]) = {
    def follow = (node: CodeNode) ⇒ node.asInstanceOf[CodeNode].name.startsWith(classPrefix)
    edge match {
      case Invocation(_, to)         ⇒ follow(to)
      case Extension(from, to)       ⇒ follow(from) || follow(to)
      case ActionToClass(_, to)      ⇒ follow(to)
      case ActionClassToResult(_, _) ⇒ true
      case JspToAction(_, _)         ⇒ true
      case JspInclude(_, _)          ⇒ true
      case Script(_, _)              ⇒ true
      case Manual(_, _)              ⇒ true
    }
  }
  /*
//  private def removeNonActionLeafs[N](graph: Graph[N], target: N, dependency: (Edge[N])=> boolean,
//  		keepInvocation: (N) => boolean): Graph[N] = {
//    def actionInvocation(g: Graph[N], edge: Edge[N]) = edge match {
////      	case Invocation(from, _) => {
////      			keepInvocation(from) || 
////                !Traverse(g, BreadthFirst[N]).follow(dependency).to(g => from).toList.isEmpty
////        }
//       	case _ => true
//    }
//    val edges = Traverse(graph, BreadthFirst[N]).follow(actionInvocation(graph,_)).to(g => target)
//	val oldEdges = Traverse(graph, BreadthFirst[N]).follow(dependency).to(g => target)
//    val newGraph = (new Graph[N] /: edges)((g, e) => {g.connect(e.from)(e.sameEdge(_,_))(e.to)})
//    if(oldEdges.toList.size == edges.toList.size) newGraph else removeNonActionLeafs(newGraph, target, dependency, keepInvocation)
//  }
*/
  private def filterFromSources[N](sources: List[N], graph: Graph[N], dependency: (Edge[N]) ⇒ Boolean,
                                   keepInvocation: (N) ⇒ Boolean)(implicit m: Manifest[N]) = {
    (new Graph[N]() /: sources)((newGraph, leafNode) ⇒ {
      //val removeFromTarget = removeNonActionLeafs(_:Graph[N], leafNode, dependency, keepInvocation)
      val traverser = new Traverse(graph, BreadthFirst[N])(m).follow(dependency).from(g ⇒ leafNode)
      val filteredGraph = (newGraph /: traverser)((g, e) ⇒ g.connect(e.from)(e.sameEdge(_, _))(e.to))
      filteredGraph
    })
  }

  private def filterToTargets[N](targets: List[N], graph: Graph[N], dependency: (Edge[N]) ⇒ Boolean)(implicit m: Manifest[N]) = {
    (new Graph[N]() /: targets)((newGraph, leafNode) ⇒ {
      //val removeFromTarget = removeNonActionLeafs(_:Graph[String], leafNode)
      val traverser = new Traverse(graph, BreadthFirst[N])(m).follow(dependency).to(g ⇒ leafNode)
      val filteredGraph = (newGraph /: traverser)((g, e) ⇒ g.connect(e.from)(e.sameEdge(_, _))(e.to))
      filteredGraph
    })
  }

  private def writeToFile[N](graph: Graph[N], styleSheet: StyleSheet[N]): Unit = {
    val ps = new PrintStream(new FileOutputStream("source.dot"))
    ps.println(new GraphVizGraph("crawler", graph, styleSheet))
    ps.close()
  }

  import com.agical.graph.Edge
  private def connect[N](graph: Graph[N], edge: (N, N) ⇒ Edge[N], from: N, to: N) = {
    graph.connect(from)(edge(_, _))(to)
  }

}
