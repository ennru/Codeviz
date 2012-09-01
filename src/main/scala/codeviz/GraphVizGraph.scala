package codeviz

import com.agical.graph._

class GraphVizGraph[N](name: String, graph: Graph[N], style: StyleSheet[N]) {
  def this(name: String, graph: Graph[N]) = this(name, graph, new StyleSheet[N] {
    override def nodeStyle(node: N) = Some(Map("label" -> node.toString))
  })
  override def toString() = {
    "digraph " + name + " {\n" +
      "  overlap = scale\n" +
      (Find.nodesIn(graph).matching(AllNodes()) map ((n) ⇒ "\t\"" + n + "\"" + styleNode(n) + ";") mkString "\n") + "\n" +
      "\n\n" +
      graph.edges.map((edge) ⇒ edge match { case Edge(left, right) ⇒ "\"" + left + "\" -> \"" + right + "\"" + styleEdge(edge) + ";" }).mkString("\n") + "\n" +
      "}"
  }

  def styleNode(n: N) = {
    style.nodeStyle(n).map(styleToString).getOrElse("")
  }

  def styleEdge(edge: Edge[N]) = {
    style.edgeStyle(edge).map(styleToString).getOrElse("")
  }

  def styleToString(attributes: Map[String, Any]) = {
    def attr(pair: (String, Any)) = pair._1 + " = \"" + value(pair._2) + "\""
    "[" + (attr(attributes.toList.head) /: attributes.toList.tail)((res, a) ⇒ { res + ", " + attr(a) }) + "]"
  }

  def value(v: Any) = v match {
    case a ⇒ "" + a
  }
}

trait StyleSheet[N] {
  def nodeStyle(node: N): Option[Map[String, Any]] = None
  def edgeStyle(edge: Edge[N]): Option[Map[String, Any]] = None
}