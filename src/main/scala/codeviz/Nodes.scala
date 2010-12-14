package codeviz

abstract case class CodeNode(val name: String)

case class ActionName(override val name: String) extends CodeNode(name)

case class MethodName(override val name: String) extends CodeNode(name)

case class Jsp(override val name: String) extends CodeNode(name)

case class IncludeFile(override val name: String) extends CodeNode(name)

case class JavaScriptFile(override val name: String) extends CodeNode(name)

case class MenuItem(override val name: String) extends CodeNode(name)

case class ServletName(override val name: String) extends CodeNode(name)
