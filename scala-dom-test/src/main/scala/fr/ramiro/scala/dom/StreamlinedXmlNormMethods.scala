package fr.ramiro.scala.dom

import scala.xml.NodeSeq
import org.scalactic.{ NormMethods, Uniformity }

trait StreamlinedXmlNormMethods extends StreamlinedXml with NormMethods {

  implicit override def streamlined[T <: NodeSeq]: Uniformity[T] = super.streamlined[T]
}

object StreamlinedXmlNormMethods extends StreamlinedXmlNormMethods
