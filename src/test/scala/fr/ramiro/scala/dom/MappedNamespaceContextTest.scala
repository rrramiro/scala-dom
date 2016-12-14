package fr.ramiro.scala.dom

import javax.xml.XMLConstants

import org.scalatest.FunSuite

import scala.collection.JavaConverters.asScalaIteratorConverter

class MappedNamespaceContextTest extends FunSuite {
  test("get prefix and get namespaceURI") {
    val customUri = "www.ramiro.fr"
    val customPrefix = "ramiro"
    val namespaceContext = new MappedNamespaceContext(Map(customPrefix -> customUri))

    assert(namespaceContext.getNamespaceURI(customPrefix) === customUri)
    assert(namespaceContext.getNamespaceURI(XMLConstants.XML_NS_PREFIX) === XMLConstants.XML_NS_URI)
    assert(namespaceContext.getNamespaceURI(XMLConstants.XMLNS_ATTRIBUTE) === XMLConstants.XMLNS_ATTRIBUTE_NS_URI)

    assert(namespaceContext.getPrefix(customUri) === customPrefix)
    assert(namespaceContext.getPrefix(XMLConstants.XML_NS_URI) === XMLConstants.XML_NS_PREFIX)
    assert(namespaceContext.getPrefix(XMLConstants.XMLNS_ATTRIBUTE_NS_URI) === XMLConstants.XMLNS_ATTRIBUTE)

    assert(namespaceContext.getPrefixes(customUri).asScala.toList === List(customPrefix))
    assert(namespaceContext.getPrefixes(XMLConstants.XML_NS_URI).asScala.toList === List(XMLConstants.XML_NS_PREFIX))
    assert(namespaceContext.getPrefixes(XMLConstants.XMLNS_ATTRIBUTE_NS_URI).asScala.toList === List(XMLConstants.XMLNS_ATTRIBUTE))
  }

  test("getNamespaceURI with null") {
    intercept[IllegalArgumentException] {
      new MappedNamespaceContext(Map.empty).getNamespaceURI(null)
    }
  }

  test("getPrefix with null") {
    intercept[IllegalArgumentException] {
      new MappedNamespaceContext(Map.empty).getPrefix(null)
    }
  }

  test("getPrefixes with null") {
    intercept[IllegalArgumentException] {
      new MappedNamespaceContext(Map.empty).getPrefixes(null)
    }
  }

}
