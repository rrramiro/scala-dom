package fr.ramiro.scala.dom

import org.scalatest.{ FunSpec, Matchers }

import scala.xml.{ Elem, Node, NodeSeq, PCData, Text, XML }

class StreamlinedXmlSpec extends FunSpec with Matchers with StreamlinedXml {

  describe("Xml Equality of Elems (after being streamlined)") {

    it("should leave already-normalized XML alone") {
      <summer></summer> should equal(<summer></summer>)(after being streamlined[Elem])
    }

    it("should zap text that is only whitespace") {

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
      <summer><day>{ Text("Hello ") }{ Text("Dude!") }</day></summer> should equal(<summer><day>Hello Dude!</day></summer>)(after being streamlined[Elem])
      <div>My name is { PCData("Harry") }</div> should equal(<div>My name is Harry</div>)(after being streamlined[Elem])
    }
  }

  describe("Xml Equality of Nodes (after being streamlined)") {

    it("should leave already-normalized XML alone") {

      ((<summer></summer>: Node) shouldEqual (<summer></summer>))(after being streamlined[Node])

      ((Text("Bla"): Node) shouldEqual (Text("Bla")))(after being streamlined[Node])
    }

    it("should zap text that is only whitespace, unless it is already a Text") {

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
      (<summer><day>{ Text("Hello ") }{ Text("Dude!") }</day></summer>: Node) should equal(<summer><day>Hello Dude!</day></summer>)(after being streamlined[Node])
      (<div>My name is { PCData("Harry") }</div>: Node) should equal(<div>My name is Harry</div>)(after being streamlined[Node])
    }
  }

  describe("Xml Equality of NodeSeq (after being streamlined)") {

    it("should leave already-normalized XML alone") {

      (<summer></summer>: NodeSeq) should equal(<summer></summer>)(after being streamlined[NodeSeq])

      (Text("Bla"): NodeSeq) should equal(Text("Bla"))(after being streamlined[NodeSeq])
    }

    it("should zap text that is only whitespace, unless it is already a Text") {

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

      (<div>{ Text("My name is ") }{ Text("Harry") }</div>: NodeSeq) should equal(<div>My name is Harry</div>)(after being streamlined[NodeSeq])
      (<summer><day>{ Text("Hello ") }{ Text("Dude!") }</day></summer>: NodeSeq) should equal(<summer><day>Hello Dude!</day></summer>)(after being streamlined[NodeSeq])
      (<div>My name is { PCData("Harry") }</div>: NodeSeq) should equal(<div>My name is Harry</div>)(after being streamlined[NodeSeq])
    }
  }

  it("should handle XML entities") {
    <root>{ ">" }</root> should equal(XML.loadString("""<root>&gt;</root>"""))(after being streamlined[Elem])
    <root>&gt;</root> should equal(XML.loadString("""<root>&gt;</root>""").head)(after being streamlined[Elem])
    <root>foo&quot;</root> should equal(<root>foo{ """"""" }</root>)(after being streamlined[Elem])
    <root>foo&quot;</root> should equal(XML.loadString("""<root>foo&quot;</root>"""))(after being streamlined[Elem])
    <root>foo&quot;</root> should equal(XML.loadString("""<root>foo"</root>""").head)(after being streamlined[Elem])
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
      case other => trimNames(other.getClass.getName) + "(" + other.toString + ")"
    }.mkString("(", ", ", ")")
  }
}
