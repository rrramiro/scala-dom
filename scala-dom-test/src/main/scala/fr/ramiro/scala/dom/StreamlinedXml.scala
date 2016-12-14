package fr.ramiro.scala.dom

import org.scalactic.Uniformity

import scala.xml._

trait StreamlinedXml {

  def streamlined[T <: NodeSeq]: Uniformity[T] = {

    def trimTextZappingEmpty(data: String): Seq[Node] =
      data.trim match {
        case "" => NodeSeq.Empty
        case trimmed => Text(trimmed)
      }

    def mergeAdjacentTextNodes(children: Seq[Node]) =
      children.foldRight[List[Node]](Nil) { (ele, acc) =>
        ele match {
          case eleTxt if eleTxt.isAtom || eleTxt.isInstanceOf[EntityRef] =>
            acc match {
              case Text(accTxt) :: tail =>
                Text(eleTxt.text + accTxt) :: tail
              case _ => Text(eleTxt.text) :: acc
            }
          case _ => ele :: acc
        }
      }

    def normalizedXml(nodeSeq: NodeSeq): NodeSeq =
      mergeAdjacentTextNodes(nodeSeq).flatMap {
        case Elem(pre, lab, md, scp, children @ _*) =>
          Elem(pre, lab, md, scp, false, normalizedXml(children): _*)
        case Text(data) =>
          trimTextZappingEmpty(data)
        case x => x
      }

    new Uniformity[T] {
      def normalized(nodeSeq: T): T = {
        normalizedXml(nodeSeq) match {
          case NodeSeq.Empty => nodeSeq
          case normalized if normalized.size == 1 => normalized.head
          case normalized => normalized
        }
      }.asInstanceOf[T]

      final def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[NodeSeq]

      final def normalizedOrSame(b: Any): Any =
        b match {
          case s: NodeSeq => StreamlinedXml.streamlined[NodeSeq].normalized(s)
          case _ => b
        }
    }
  }
}

object StreamlinedXml extends StreamlinedXml