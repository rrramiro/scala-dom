package fr.ramiro.scala.dom

import javax.xml.XMLConstants
import javax.xml.namespace.NamespaceContext

import scala.collection.JavaConverters.asJavaIteratorConverter

object MappedNamespaceContext {
  def apply(namespaceContextMap: Map[String, String]): Option[NamespaceContext] = {
    if (namespaceContextMap.isEmpty) {
      None
    } else {
      Some(new MappedNamespaceContext(namespaceContextMap))
    }
  }
}

class MappedNamespaceContext(baseNamespaceContextMap: Map[String, String]) extends NamespaceContext {
  private val namespaceContextMap = baseNamespaceContextMap ++ Map(
    XMLConstants.XML_NS_PREFIX -> XMLConstants.XML_NS_URI,
    XMLConstants.XMLNS_ATTRIBUTE -> XMLConstants.XMLNS_ATTRIBUTE_NS_URI,
    XMLConstants.DEFAULT_NS_PREFIX -> XMLConstants.NULL_NS_URI
  )

  private val reverseNamespaceContextMap = namespaceContextMap.map(_.swap)

  override def getPrefixes(namespaceURI: String): java.util.Iterator[_] = Option(namespaceURI) match {
    case Some(nsUri) =>
      namespaceContextMap.collect { case (key, value) if value == nsUri => key }.iterator.asJava
    case _ => throw new IllegalArgumentException("Namespace URI cannot be null.")
  }

  override def getPrefix(namespaceURI: String): String = Option(namespaceURI) match {
    case Some(nsUri) =>
      reverseNamespaceContextMap.getOrElse(nsUri, throw new Exception(s"Unknown namespaceURI '$nsUri'"))
    case _ => throw new IllegalArgumentException("Namespace URI cannot be null.")
  }

  override def getNamespaceURI(prefix: String): String = Option(prefix) match {
    case Some(pref) =>
      namespaceContextMap.getOrElse(pref, throw new Exception(s"Unknown prefix '$pref'"))
    case _ => throw new IllegalArgumentException("Prefix cannot be null.")
  }
}
