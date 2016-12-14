package fr.ramiro.scala.dom

import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.xml.{Elem, Node, NodeSeq, Text}

class StreamlinedXmlSpec extends FunSpec with Matchers with StreamlinedXml {

  object `Xml Equality of Elems (after being streamlined)` {

    def `should leave already-normalized XML alone`() {
      <summer></summer> should equal(<summer></summer>)(after being streamlined[Elem])
    }

    def `should zap text that is only whitespace`() {

      <summer> </summer> should equal(<summer></summer>)(after being streamlined[Elem])

      <summer>
      </summer> should equal(<summer></summer>)(after being streamlined[Elem])

      <summer>
        <day></day>
      </summer> should equal(<summer><day></day></summer>)(after being streamlined[Elem])

      <summer><day></day></summer> should equal(
        <summer>
          <day></day>
        </summer>
      )(after being streamlined[Elem])

      <summer><day>Dude!</day></summer> should equal(
        <summer>
          <day>
            Dude!
          </day>
        </summer>
      )(after being streamlined[Elem])

      <div>{ Text("My name is ") }{ Text("Harry") }</div> should equal(<div>My name is Harry</div>)(after being streamlined[Elem])

      assert(<div>My name is Harry</div>.toString() === streamlined[NodeSeq].normalized(<div>{ Text("My name is ") }{ Text("Harry") }</div>).toString())
    }
  }

  object `Xml Equality of Nodes (after being streamlined)` {

    def `should leave already-normalized XML alone`() {

      ((<summer></summer>: Node) shouldEqual <summer></summer>)(after being streamlined[Node])

      ((Text("Bla"): Node) shouldEqual Text("Bla"))(after being streamlined[Node])
    }

    def `should zap text that is only whitespace, unless it is already a Text`() {

      (<summer> </summer>: Node) should equal(<summer></summer>)(after being streamlined[Node])

      (<summer>
       </summer>: Node) should equal(<summer></summer>)(after being streamlined[Node])

      (<summer>
         <day></day>
       </summer>: Node) should equal(<summer><day></day></summer>)(after being streamlined[Node])

      <summer><day></day></summer> should equal(
        <summer>
          <day></day>
        </summer>: Node
      )(after being streamlined[Node])

      <summer><day>Dude!</day></summer> should equal(
        <summer>
          <day>
            Dude!
          </day>
        </summer>: Node
      )(after being streamlined[Node])

      (Text("   "): Node) should equal(Text("   "))(after being streamlined[Node])

      (<div>{ Text("My name is ") }{ Text("Harry") }</div>: Node) should equal(<div>My name is Harry</div>)(after being streamlined[Node])
    }
  }

  object `Xml Equality of NodeSeq (after being streamlined)` {

    def `should leave already-normalized XML alone`() {

      (<summer></summer>: NodeSeq) should equal(<summer></summer>)(after being streamlined[NodeSeq])

      (Text("Bla"): NodeSeq) should equal(Text("Bla"))(after being streamlined[NodeSeq])
    }

    def `should zap text that is only whitespace, unless it is already a Text`() {

      (<summer> </summer>: NodeSeq) should equal(<summer></summer>)(after being streamlined[NodeSeq])

      (<summer>
       </summer>: NodeSeq) should equal(<summer></summer>)(after being streamlined[NodeSeq])

      (<summer>
         <day></day>
       </summer>: NodeSeq) should equal(<summer><day></day></summer>)(after being streamlined[NodeSeq])

      <summer><day></day></summer> should equal(
        <summer>
          <day></day>
        </summer>: NodeSeq
      )(after being streamlined[NodeSeq])

      <summer><day>Dude!</day></summer> should equal(
        <summer>
          <day>
            Dude!
          </day>
        </summer>: NodeSeq
      )(after being streamlined[NodeSeq])

      (Text("   "): NodeSeq) should equal(Text("   "))(after being streamlined[NodeSeq])

      (Text("   ") ++ Text("some text    "): NodeSeq) should equal(Text("some text"))(after being streamlined[NodeSeq])

      (<div>{ Text("My name is ") }{ Text("Harry") }</div>: NodeSeq) should equal(<div>My name is Harry</div>)(after being streamlined[NodeSeq])
    }

    def `keep order of nodes`(): Unit = {
      val xml: NodeSeq =
        <presentation>
          <formulation>Gluten-free</formulation>
          <presentationInfo>
            per 100␣mL
            <acbsIndications title="AcbsIndications" class="subheading">
              <p><xref type="bookmark" idref="PHP104168"/></p>
            </acbsIndications>
          </presentationInfo>
          <dmdIds class="list">
            <dmdId type="AMP">218211000001100</dmdId>
            <dmdId type="AMP">9379011000001108</dmdId>
            <dmdId type="AMP">751411000001105</dmdId>
          </dmdIds>
          <presentationNote>
            <p>Powder provides: protein equivalent 10 g, carbohydrate 58.5 g, fat 14 g, energy 1683 kJ (400 kcal)/100 g.</p>
            <p>To flavour unflavoured products, see<xref type="bookmark" idref="PHP103758"/>.</p>
          </presentationNote>
        </presentation>

      val xmlExpected: NodeSeq = <presentation><formulation>Gluten-free</formulation><presentationInfo>per 100␣mL<acbsIndications title="AcbsIndications" class="subheading"><p><xref type="bookmark" idref="PHP104168"></xref></p></acbsIndications></presentationInfo><dmdIds class="list"><dmdId type="AMP">218211000001100</dmdId><dmdId type="AMP">9379011000001108</dmdId><dmdId type="AMP">751411000001105</dmdId></dmdIds><presentationNote><p>Powder provides: protein equivalent 10 g, carbohydrate 58.5 g, fat 14 g, energy 1683 kJ (400 kcal)/100 g.</p><p>To flavour unflavoured products, see<xref type="bookmark" idref="PHP103758"></xref>.</p></presentationNote></presentation>

      assert(streamlined[NodeSeq].normalized(xml).mkString === xmlExpected.mkString)
    }
  }

  def serializeStructure(parent: NodeSeq): String = {
    def trimNames(className: String) = {
      className.indexOf("$") match {
        case -1 =>
          className.substring(className.lastIndexOf(".") + 1)
        case last =>
          className.substring(className.lastIndexOf(".") + 1, last)
      }
    }
    trimNames(parent.getClass.getName) + parent.map {
      case node: Node if node.child.nonEmpty => serializeStructure(node.child: NodeSeq)
      case other                             => trimNames(other.getClass.getName) + "(" + other.toString + ")"
    }.mkString("(", ", ", ")")
  }
}
