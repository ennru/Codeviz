package codeviz

import org.objectweb.asm.commons._
import org.objectweb.asm._
import com.agical.graph._
import java.net.URL
import java.io.File
import java.io.FileInputStream

object ClassCrawler {

  def crawl(visitorFactory: (Graph[CodeNode]) ⇒ GraphVisitor[CodeNode], classDirs: File*): Graph[CodeNode] = {
    (new Graph[CodeNode]() /: classDirs)(crawl(_, visitorFactory, _))
  }

  def crawl(graph: Graph[CodeNode], visitorFactory: (Graph[CodeNode]) ⇒ GraphVisitor[CodeNode], classDirs: List[File]): Graph[CodeNode] = {
    (graph /: classDirs)(crawl(_, visitorFactory, _))
  }

  def crawl(visitorFactory: (Graph[CodeNode]) ⇒ GraphVisitor[CodeNode], classDir: URL): Graph[CodeNode] = {
    crawl(visitorFactory, new File(classDir.toURI))
  }

  def crawl(graph: Graph[CodeNode], visitorFactory: (Graph[CodeNode]) ⇒ GraphVisitor[CodeNode], file: File): Graph[CodeNode] = {
    if (file.isDirectory) crawlDir(graph, visitorFactory, file)
    else crawlFile(graph, visitorFactory, file)
  }

  def crawlDir(graph: Graph[CodeNode], visitorFactory: (Graph[CodeNode]) ⇒ GraphVisitor[CodeNode], file: File) = {
    (graph /: file.listFiles)(crawl(_, visitorFactory, _))
  }

  def crawlFile(graph: Graph[CodeNode], visitorFactory: (Graph[CodeNode]) ⇒ GraphVisitor[CodeNode], file: File): Graph[CodeNode] = {
    if (!file.getName.endsWith(".class")) return graph
    val is = new FileInputStream(file)
    try {
      try {
        val reader = new ClassReader(is)
        val updater = visitorFactory(graph)
        reader.accept(updater, 0)
        updater.graph
      } catch {
        case e ⇒ println(file); throw e
      }
    } finally {
      is.close()
    }
  }
}

class GraphVisitor[N](var graph: Graph[N]) extends ClassVisitor(Opcodes.V1_7)

class GraphMethodUpdater(g: Graph[CodeNode]) extends GraphVisitor(g) {
  private var className = ""
  def toClassName(n: String) = {
    (if (n.endsWith("Bean") || (n.endsWith("Impl"))) n.substring(0, n.length - 4)
    else if (n.endsWith("Bean2")) n.substring(0, n.length - 5) + "2"
    else n).replace('/', '.')
  }
  override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]) {
    className = toClassName(name)
  }
  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
    graph = graph.create(MethodName(className + "_" + name))

    new MethodVisitor(Opcodes.V1_7) {
      override def visitMethodInsn(opCode: Int, owner: String, targetName: String, desc: String) {
        graph = connect(className + "_" + name, toClassName(owner) + "_" + targetName, (from: CodeNode, to: CodeNode) ⇒ Invocation(from, to))
      }
    }
  }

  import com.agical.graph.Edge
  def connect(from: CodeNode, to: CodeNode, edge: (CodeNode, CodeNode) ⇒ Edge[CodeNode]) = {
    graph.connect(from)(edge(_, _))(to)
  }
  def connect(from: String, to: String, edge: (CodeNode, CodeNode) ⇒ Edge[CodeNode]) = {
    graph.connect(MethodName(from))(edge(_, _))(MethodName(to))
  }
}

class GraphUpdater(g: Graph[String]) extends GraphVisitor(g) {
  private var className = ""
  override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]) {
    def toClassName(n: String) = n.replace('/', '.')
    className = toClassName(name)
    val superClassName = toClassName(superName)
    graph = if (shouldConnect(className)) graph.create(className, "name" -> className) else graph
    graph = if (className.endsWith("Bean")) {
      graph = connect(className, className.substring(0, className.length - 4), Extension(_, _))
      connect(className.substring(0, className.length - 4), className, Invocation(_, _))
    } else graph
    graph = connect(className, superClassName, Extension(_, _))
    graph = connect(superClassName, className, Invocation(_, _))
    graph = (graph /: interfaces)((g, itf) ⇒ { connect(className, toClassName(itf), Extension(_, _)) })
  }

  override def visitField(access: Int, name: String, desc: String, signature: String, value: Object): FieldVisitor = {
    val fieldType = Type.getType(desc)
    if (fieldType.getSort == Type.OBJECT) {
      graph = connect(className, fieldType.getClassName(), Invocation(_, _))
    }
    null
  }

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
    for (arg ← Type.getArgumentTypes(desc)) {
      if (arg.getSort == Type.OBJECT) {
        graph = connect(className, arg.getClassName(), Invocation(_, _))
      }
    }
    val returnType = Type.getReturnType(desc)
    graph = if (returnType.getSort == Type.VOID) graph else connect(className, returnType.getClassName(), Invocation(_, _))
    new MethodVisitor(Opcodes.V1_7) {
      override def visitTypeInsn(opcode: Int, desc: String) {
        if (Opcodes.NEW == opcode) {
          graph = connect(className, desc.replace('/', '.'), Invocation(_, _))
        } else graph
      }
    }
  }

  def shouldConnect(element: String) = !element.contains("$") && !element.startsWith("java") && !element.startsWith("scala")

  import com.agical.graph.Edge
  def connect(from: String, to: String, edge: (String, String) ⇒ Edge[String]) = {
    if (shouldConnect(from) && shouldConnect(to)) graph.connect(from)(edge(_, _))(to)
    else graph
  }
}

