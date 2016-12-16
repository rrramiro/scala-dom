package fr.ramiro.scala.dom

import javax.xml.namespace.NamespaceContext
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.{ XPathExpression, XPathFactory }

import net.sf.saxon.lib.NamespaceConstant

import scala.collection.mutable
import scala.language.implicitConversions

object W3cNodeSeq {
  val empty = new W3cNodeSeq(Seq.empty)

  val xPathFactory = XPathFactory.newInstance(NamespaceConstant.OBJECT_MODEL_SAXON, classOf[net.sf.saxon.xpath.XPathFactoryImpl].getName, getClass.getClassLoader)

  private def compileXPath(expression: String, namespaceContext: Option[NamespaceContext] = None): XPathExpression = {
    val xpath = xPathFactory.newXPath()
    namespaceContext.foreach { xpath.setNamespaceContext }
    xpath.compile(expression)
  }

}

case class W3cNodeSeq(delegate: Seq[org.w3c.dom.Node]) extends Seq[org.w3c.dom.Node] with org.w3c.dom.NodeList {
  override def getLength: Int = this.length
  override def item(index: Int): org.w3c.dom.Node = this.apply(index)
  override def length: Int = delegate.length
  override def apply(idx: Int): org.w3c.dom.Node = delegate.apply(idx)
  override def iterator: Iterator[org.w3c.dom.Node] = delegate.iterator

  import W3cNodeSeq.compileXPath

  private val DEFAULT_TAG_PATTERN = """^\p{Alpha}\p{Alnum}*$""".r

  def \\(expression: String): W3cNodeSeq = {
    if (expression == "@" || expression.isEmpty) {
      throw new IllegalArgumentException("Expression is missing")
    }
    W3cNodeSeq(
      expression match {
        case DEFAULT_TAG_PATTERN() => delegate.flatMap {
          case node: org.w3c.dom.Element => node.getElementsByTagName(expression)
          case _ => W3cNodeSeq.empty
        }
        case _ =>
          findNodesByXpath(s".//$expression")
      }
    )
  }

  def \@(expression: String): String = {
    if (expression.isEmpty) {
      throw new IllegalArgumentException("Attribut name is missing")
    }
    delegate.map { node =>
      Option(node.getAttributes.getNamedItem(expression)).fold("") {
        _.getTextContent
      }
    }.mkString
  }

  def \(expression: String): W3cNodeSeq = W3cNodeSeq(delegate.flatMap { node =>
    if (expression == "_") {
      node.child
    } else if (expression.startsWith("@")) {
      val expr = expression.stripPrefix("@")
      if (expr.isEmpty) {
        throw new IllegalArgumentException("Attribute name is missing")
      }
      Option(node.getAttributes.getNamedItem(expr))
    } else {
      W3cNodeSeq(node.getChildNodes.filter { _.getNodeName == expression })
    }
  })

  //TODO Maybe should throw an exception if delegate size is > 1
  def label: String = delegate.headOption match {
    case Some(node) => node.getNodeName
    case _ => ""
  }

  override def addString(builder: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    var first = true
    builder append start
    for (node <- delegate; x = nodeToString(node)) {
      if (first) {
        builder append x
        first = false
      } else {
        builder append sep
        builder append x
      }
    }
    builder append end
    builder
  }

  def findParent(expression: String): W3cNodeSeq = W3cNodeSeq(delegate.flatMap { node =>
    findParentRec(node, expression)
  })

  private def findParentRec(node: org.w3c.dom.Node, expression: String): Option[org.w3c.dom.Node] = {
    Option(node.getParentNode) match {
      case aParent @ Some(parent) if parent.getNodeName == expression => aParent
      case Some(parent) => findParentRec(parent, expression)
      case _ => None
    }
  }

  def namespace: String = delegate.map { n => Option(n.getNamespaceURI).getOrElse("") }.mkString

  def text: String = delegate.map { _.getTextContent }.mkString

  //TODO
  def attributes = ???

  def xml_sameElements[A](that: Iterable[A]): Boolean = ???

  def delete() = delegate.foreach {
    case attr: org.w3c.dom.Attr =>
      attr.getOwnerElement.removeAttributeNode(attr)
    case node =>
      node.getParentNode.removeChild(node)
  }

  def replaceWith(newNode: org.w3c.dom.Node) = delegate.foreach { node =>
    node.getParentNode.replaceChild(node.getOwnerDocument.importNode(newNode, true), node)
  }

  def appendSibling(newNode: org.w3c.dom.Node) = delegate.foreach { node =>
    node.getParentNode.insertBefore(node.getOwnerDocument.importNode(newNode, true), node.getNextSibling)
  }

  def prependSibling(newNode: org.w3c.dom.Node) = delegate.foreach { node =>
    node.getParentNode.insertBefore(node.getOwnerDocument.importNode(newNode, true), node)
  }

  def setAttribute(key: String, value: String) = delegate.foreach {
    case node: org.w3c.dom.Element =>
      node.setAttribute(key, value)
  }

  def toDocument(namespaceAware: Boolean = false): org.w3c.dom.Document = {
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(namespaceAware)
    val newDocument = factory.newDocumentBuilder().newDocument()
    delegate.foreach { node =>
      newDocument.appendChild(newDocument.importNode(node, true))
    }
    newDocument
  }

  def findNodesByXpath(xpathQuery: String, namespaceContextMap: Map[String, String] = Map.empty): W3cNodeSeq = {
    val xPathExpression = compileXPath(xpathQuery, MappedNamespaceContext(namespaceContextMap))
    W3cNodeSeq(delegate.flatMap { xPathExpression.evaluateNodeSet })
  }

  def findNodeByXpath(xpathQuery: String, namespaceContextMap: Map[String, String] = Map.empty): Option[org.w3c.dom.Node] = {
    val xPathExpression = compileXPath(xpathQuery, MappedNamespaceContext(namespaceContextMap))
    delegate.flatMap { xPathExpression.evaluateNode }.headOption
  }

  def child: W3cNodeSeq = W3cNodeSeq(delegate.flatMap { _.getChildNodes })

  def removeWithXPath(expression: String, namespaceContextMap: Map[String, String] = Map.empty): Unit = {
    val xPathExpression = compileXPath(expression, MappedNamespaceContext(namespaceContextMap))
    delegate.foreach { node =>
      xPathExpression.evaluateNodeSet(node).foreach { _.delete() }
    }
  }

  def xml_==(n: W3cNodeSeq): Boolean = {
    import org.xmlunit.diff._
    import org.xmlunit.builder.Input
    val differences = mutable.ListBuffer[Comparison]()
    val e = new DOMDifferenceEngine()
    e.setDifferenceEvaluator(DifferenceEvaluators.ignorePrologDifferences())
    e.addDifferenceListener((comparison: Comparison, outcome: ComparisonResult) => if (outcome == ComparisonResult.DIFFERENT) { differences += comparison })
    e.compare(Input.fromDocument(this.toDocument()).build(), Input.fromDocument(n.toDocument()).build())
    differences.isEmpty
  }

  def asScala: scala.xml.NodeSeq = delegate.map { convertNodeToScalaNode }

  def streamLined() = delegate.foreach { node =>
    //TODO validate if the case exists
    //mergeContiguousText(Seq(node))
    node.findNodesByXpath("//text()").foreach { spacedNodes =>
      val processedContent = spacedNodes.getTextContent.replaceAll("[\r\n]", " ").replaceAll("\\s{2,}", " ")
      if (processedContent.isEmpty) {
        spacedNodes.delete()
      } else {
        spacedNodes.setTextContent(processedContent)
      }
    }
  }

  def removeNamespaceAttributes(): Unit = delegate.foreach { node =>
    W3cNodeSeq(node.getAttributes.filter(_.getNodeName.contains(':'))).delete()
    node.getChildNodes.removeNamespaceAttributes()
  }

  def copyNodes(namespaceAware: Boolean = false): W3cNodeSeq = {
    toDocument(namespaceAware).child
  }

  private def mergeContiguousText(children: Seq[org.w3c.dom.Node]): Unit = {
    children match {
      case first :: second :: tail if first.isInstanceOf[org.w3c.dom.Text] && second.isInstanceOf[org.w3c.dom.Text] =>
        first.setTextContent(first.getTextContent + second.getTextContent)
        second.delete()
        mergeContiguousText(first :: tail)
      case head :: tail =>
        mergeContiguousText(head.child)
        mergeContiguousText(tail)
      case _ =>
    }
  }

}
