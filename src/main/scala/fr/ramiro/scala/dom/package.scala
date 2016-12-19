package fr.ramiro.scala

import java.io.{ ByteArrayInputStream, File, InputStreamReader, StringWriter }
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.{ OutputKeys, TransformerException, TransformerFactory }
import javax.xml.xpath._

import _root_.com.sun.org.apache.xalan.internal.xsltc.trax.DOM2SAX
import org.w3c.dom.Document
import org.xml.sax.InputSource
import scala.language.{ implicitConversions, reflectiveCalls }
import scala.xml.parsing.NoBindingFactoryAdapter
import scala.xml.{ NodeSeq, XML }

package object dom {

  implicit class XPathExpressionWrapper(xPathExpression: XPathExpression) {
    def evaluateNodeSet(node: org.w3c.dom.Node): org.w3c.dom.NodeList = {
      xPathExpression.evaluate(node, XPathConstants.NODESET).asInstanceOf[org.w3c.dom.NodeList]
    }
    def evaluateNode(node: org.w3c.dom.Node): Option[org.w3c.dom.Node] = {
      Option(xPathExpression.evaluate(node, XPathConstants.NODE).asInstanceOf[org.w3c.dom.Node])
    }
  }

  private type Nodes = {
    def item(index: Int): org.w3c.dom.Node
    def getLength: Int
  }

  private def nodesToW3cNodeSeq(nodeList: Nodes): W3cNodeSeq = {
    new W3cNodeSeq(Option(nodeList).toSeq.flatMap { nList =>
      (0 until nList.getLength).map { nList.item }
    })
  }

  implicit def convertNodeListToW3cNodeSeq(nodeList: org.w3c.dom.NodeList): W3cNodeSeq = nodesToW3cNodeSeq(nodeList.asInstanceOf[Nodes])

  implicit def convertNamedNodeMapToW3cNodeSeq(nodeList: org.w3c.dom.NamedNodeMap): W3cNodeSeq = nodesToW3cNodeSeq(nodeList.asInstanceOf[Nodes])

  implicit def convertNodeToW3cNodeSeq(n: org.w3c.dom.Node): W3cNodeSeq = new W3cNodeSeq(Option(n).toSeq)

  def loadFile(file: File, namespaceAware: Boolean = false): org.w3c.dom.Document = {
    val dbf = DocumentBuilderFactory.newInstance()
    dbf.setNamespaceAware(namespaceAware)
    dbf.newDocumentBuilder().parse(file)
  }

  def convertStringToNode(xml: String, namespaceAware: Boolean = false): org.w3c.dom.Node = {
    val baos = new ByteArrayInputStream(xml.getBytes("UTF-8"))
    val reader = new InputStreamReader(baos, "UTF-8")
    val is = new InputSource(reader)
    is.setEncoding("UTF-8")
    val documentBuilderFactory = DocumentBuilderFactory.newInstance()
    documentBuilderFactory.setNamespaceAware(namespaceAware)
    val document: Document = documentBuilderFactory.newDocumentBuilder().parse(is)
    document.getDocumentElement
  }

  def convertNodeToScalaNode(dom: org.w3c.dom.Node): scala.xml.Node = {
    dom match {
      case text: org.w3c.dom.Text =>
        scala.xml.Text(text.getWholeText)
      case comment: org.w3c.dom.Comment =>
        scala.xml.Comment(comment.getTextContent)
      case procInst: org.w3c.dom.ProcessingInstruction =>
        scala.xml.ProcInstr(procInst.getTarget, procInst.getData)
      case procInst: org.w3c.dom.EntityReference =>
        scala.xml.EntityRef(procInst.getNodeName)
      case _ =>
        val dom2sax = new DOM2SAX(dom)
        val adapter = new NoBindingFactoryAdapter
        dom2sax.setContentHandler(adapter)
        dom2sax.parse()
        assert(adapter.rootElem != null, s"${dom.getClass.getName} couldn't be transformed to scala.xml.Node")
        adapter.rootElem
    }
  }

  private val SAXON = classOf[net.sf.saxon.TransformerFactoryImpl]

  def nodeToString(inputNode: org.w3c.dom.Node, transformerFactoryClazz: Class[_ <: TransformerFactory] = SAXON): String = inputNode match {
    case text: org.w3c.dom.Text =>
      text.getTextContent
    case node =>
      val sw: StringWriter = new StringWriter
      try {
        val factory: TransformerFactory = TransformerFactory.newInstance(transformerFactoryClazz.getName, this.getClass.getClassLoader)
        val t = factory.newTransformer
        t.setOutputProperty(OutputKeys.METHOD, "xml")
        t.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
        t.setOutputProperty(OutputKeys.INDENT, "no")

        t.transform(new DOMSource(node), new StreamResult(sw))
      } catch {
        case te: TransformerException => throw te
      }
      sw.toString
  }

  def stringToScalaNode(xml: String): NodeSeq = {
    XML.loadString("<holder>" + xml + "</holder>").child
  }

  def convertScalaNodeToNode(scalaNode: scala.xml.Node, namespaceAware: Boolean = false): org.w3c.dom.Node = {
    val builderFactory = DocumentBuilderFactory.newInstance()
    builderFactory.setNamespaceAware(namespaceAware)
    buildNodeFromScalaNode(scalaNode, builderFactory.newDocumentBuilder().newDocument())
  }

  private def buildNodeFromScalaNode(node: scala.xml.Node, parent: org.w3c.dom.Node): org.w3c.dom.Node = {
    val doc: org.w3c.dom.Document = parent match {
      case document: org.w3c.dom.Document => document
      case _ => parent.getOwnerDocument
    }
    val jnode = node match {
      case a if a.isAtom || a.isInstanceOf[scala.xml.EntityRef] => doc.createTextNode(a.text)
      case c: scala.xml.Comment => doc.createComment(c.commentText)
      //case er: scala.xml.EntityRef => doc.createEntityReference(er.entityName)
      case pi: scala.xml.ProcInstr => doc.createProcessingInstruction(pi.target, pi.proctext)
      case e: scala.xml.Node =>
        val jn = Option(e.namespace) match {
          case Some(namespace) =>
            val (prefix, postfix) = Option(e.prefix).map(prefix => (prefix + ":") -> (":" + prefix)).getOrElse("" -> "")
            val jns = doc.createElementNS(namespace, prefix + e.label)
            jns.setAttribute("xmlns" + postfix, namespace)
            jns
          case None =>
            doc.createElement(e.label)
        }
        Option(e.scope) match {
          case Some(scope) =>
            jn.setAttribute("xmlns:" + scope.prefix, scope.uri)
          case _ =>
        }
        e.attributes foreach { a =>
          jn.setAttribute(if (a.isPrefixed) a.prefixedKey else a.key, a.value.text)
        }
        e.child.foreach { buildNodeFromScalaNode(_, jn) }
        jn
    }
    (parent, jnode) match {
      case (_: org.w3c.dom.Document, _: org.w3c.dom.Text) => jnode
      case _ => parent.appendChild(jnode)
    }
  }

  implicit class ScalaNodeWrapper(scalaNode: scala.xml.Node) {
    def asW3cNode(namespaceAware: Boolean = false): org.w3c.dom.Node = {
      convertScalaNodeToNode(scalaNode, namespaceAware)
    }
  }
}
