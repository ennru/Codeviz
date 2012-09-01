package codeviz

class JspCrawler(val webappName: String, val rootDirName: String, val subDir: String) {

  import com.agical.graph._
  import java.io.{ File, FilenameFilter, FileFilter, FileReader, BufferedReader }

  private var graph: Graph[CodeNode] = null

  val dotAction = ".action"

  def scanJsps(inGraph: Graph[CodeNode]): Graph[CodeNode] = {
    graph = inGraph
    val webappDir = new File(rootDirName + subDir)
    def acceptJspFiles = (file: File) ⇒
      file.getName.endsWith(".jsp") || file.getName.endsWith(".inc") || file.getName.endsWith(".js")
    def acceptDirectories = (file: File) ⇒ file.isDirectory
    def handleJspFile = (file: File) ⇒ {
      val absPath = toWebappPath(file)
      scanJsp(absPath, file)
    }
    def handleDirectory: (File) ⇒ Unit = (dir: File) ⇒ {
      ScanDirectory(dir,
        acceptJspFiles,
        handleJspFile)
      ScanDirectory(dir,
        acceptDirectories,
        handleDirectory)
    }
    ScanDirectory(webappDir,
      acceptJspFiles,
      handleJspFile)
    ScanDirectory(webappDir,
      acceptDirectories,
      handleDirectory)
    graph
  }

  private def toWebappPath(file: File) = file.getCanonicalPath.substring(rootDirName.length).replace('\\', '/')

  private def scanJsp(jspName: String, file: File): Unit = {
    val is = new BufferedReader(new FileReader(file))
    var line = is.readLine()
    while (line != null) {
      parseJspLine(file, jspName, line)
      line = is.readLine
    }
    is.close()
  }

  private def webappPrefix(name: String) = if (name.startsWith("/")) name else "/" + webappName + "/" + name

  private def resolveRelativePath(jspFile: File, relativePath: String) = {
    val f = jspFile.getParentFile;
    val file = new File(f, relativePath)
    toWebappPath(file)
  }

  private def parseJspLine(file: File, jspName: String, line: String): Unit = {
    //val absoluteJspName = if (jspName.startsWith("/")) jspName else "/" + webappName + "/" + jspName
    val jspNode =
      if (jspName.endsWith(".jsp")) Jsp(jspName)
      else if (jspName.endsWith(".inc")) IncludeFile(jspName)
      else if (jspName.endsWith(".js")) JavaScriptFile(jspName)
      else throw new RuntimeException("unkown node type for " + jspName)
    def tester = testLine(jspName, line)_
    tester(dotAction, ".*[\"']([\\w/]*)\\.action.*",
      (target) ⇒ connectGraph(jspNode, ActionName(webappPrefix(target + dotAction)), JspToAction(_, _)))
    tester("<script", "\\s*<script type=\"text/javascript\" src=[\"'](js/[\\w/]*\\.js).*",
      (target) ⇒ connectGraph(jspNode, JavaScriptFile("/" + target), Script(_, _)))
    // handle relative paths in include
    tester("<%@ include", "\\s*<%@ include file=\"([\\w/]*\\.inc).*",
      (target) ⇒ connectGraph(jspNode, IncludeFile(target), JspInclude(_, _)))
    tester("<%@include", "\\s*<%@include file=\"([\\w/]*\\.inc).*",
      (target) ⇒ connectGraph(jspNode, IncludeFile(target), JspInclude(_, _)))
    // handle relative paths in include
    tester("<%@ include", "\\s*<%@ include file=\"(\\.[\\.\\w/]*\\.inc).*",
      (target) ⇒ connectGraph(jspNode, IncludeFile(resolveRelativePath(file, target)), JspInclude(_, _)))
    tester("<%@include", "\\s*<%@include file=\"(\\.[\\.\\w/]*\\.inc).*",
      (target) ⇒ connectGraph(jspNode, IncludeFile(resolveRelativePath(file, target)), JspInclude(_, _)))
  }

  private def testLine(jspName: String, line: String)(contained: String, pattern: String, connect: (String) ⇒ Unit): Unit = {
    import java.util.regex.Pattern
    if (line.indexOf(contained) != -1) {
      val p = Pattern.compile(pattern)
      val m = p.matcher(line)
      if (m.matches()) {
        val matched = m.group(1)
        connect(matched)
      } else {
        printf("%s: unmatched line: %s\n", jspName, line)
      }
    }
  }

  import com.agical.graph.Edge
  private def connectGraph(from: CodeNode, to: CodeNode, edge: (CodeNode, CodeNode) ⇒ Edge[CodeNode]) = {
    graph = graph.connect(from)(edge(_, _))(to)
  }

}
