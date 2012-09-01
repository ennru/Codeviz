package codeviz

import com.agical.graph._

case class Invocation[N](val left: N, val right: N) extends Edge[N](left, right) {
  override def sameEdge(from: N, to: N) = Invocation(from, to)
}
case class Extension[N](val left: N, val right: N) extends Edge[N](left, right) {
  override def sameEdge(from: N, to: N) = Extension(from, to)
}
case class ActionToClass[N](val left: N, val right: N) extends Edge[N](left, right) {
  override def sameEdge(from: N, to: N) = ActionToClass(from, to)
}
case class ActionClassToResult[N](val left: N, val right: N) extends Edge[N](left, right) {
  override def sameEdge(from: N, to: N) = ActionClassToResult(from, to)
}
case class JspToAction[N](val left: N, val right: N) extends Edge[N](left, right) {
  override def sameEdge(from: N, to: N) = JspToAction(from, to)
}
case class JspInclude[N](val left: N, val right: N) extends Edge[N](left, right) {
  override def sameEdge(from: N, to: N) = JspInclude(from, to)
}
case class Script[N](val left: N, val right: N) extends Edge[N](left, right) {
  override def sameEdge(from: N, to: N) = Script(from, to)
}
case class Manual[N](val left: N, val right: N) extends Edge[N](left, right) {
  override def sameEdge(from: N, to: N) = Manual(from, to)
}
