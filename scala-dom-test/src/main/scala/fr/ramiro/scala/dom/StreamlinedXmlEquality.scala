package fr.ramiro.scala.dom

import scala.xml.NodeSeq
import org.scalactic.{Equality, Uniformity}


trait StreamlinedXmlEquality {
  implicit def streamlinedXmlEquality[T <: NodeSeq]: Equality[T] = {
    new Equality[T] {
      val xu: Uniformity[T] = StreamlinedXml.streamlined[T]
      def areEqual(a: T, b: Any): Boolean = {
        xu.normalized(a) == xu.normalizedOrSame(b)
      }
    }
  }
}

object StreamlinedXmlEquality extends StreamlinedXmlEquality
