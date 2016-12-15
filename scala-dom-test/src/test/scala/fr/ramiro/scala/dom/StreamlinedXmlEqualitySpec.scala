package fr.ramiro.scala.dom

import org.scalatest.{ FunSpec, Matchers }
import scala.xml.{ Node, NodeSeq, PCData, Text }
import fr.ramiro.scala.dom.StreamlinedXmlEquality._

class StreamlinedXmlEqualitySpec extends FunSpec with Matchers {

  describe("Streamlined Xml Equality of Elems") {

    it("should leave already-normalized XML alone") {
      <summer></summer> should equal(<summer></summer>)
    }

    it("should zap text that is only whitespace") {

      <summer> </summer> should equal(<summer></summer>)

      <summer>
      </summer> should equal(<summer></summer>)

      <summer>
        <day></day>
      </summer> should equal(<summer><day></day></summer>)

      <summer><day></day></summer> should equal(
        <summer>
          <day></day>
        </summer>
      )

      <summer><day>Dude!</day></summer> should equal(
        <summer>
          <day>
            Dude!
          </day>
        </summer>
      )

      <div>{ Text("My name is ") }{ Text("Harry") }</div> should equal(<div>My name is Harry</div>)
      <summer><day>{ Text("Hello ") }{ Text("Dude!") }</day></summer> should equal(<summer><day>Hello Dude!</day></summer>)
      <div>My name is { PCData("Harry") }</div> should equal(<div>My name is Harry</div>)
    }
  }

  describe("Streamlined Xml Equality of Nodes") {

    it("should leave already-normalized XML alone") {

      (<summer></summer>: Node) shouldEqual (<summer></summer>)

      (Text("Bla"): Node) shouldEqual (Text("Bla"))
    }

    it("should zap text that is only whitespace, unless it is already a Text") {

      (<summer> </summer>: Node) should equal(<summer></summer>)

      (<summer>
       </summer>: Node) should equal(<summer></summer>)

      (<summer>
         <day></day>
       </summer>: Node) should equal(<summer><day></day></summer>)

      <summer><day></day></summer> should equal(
        <summer>
          <day></day>
        </summer>: Node
      )

      <summer><day>Dude!</day></summer> should equal(
        <summer>
          <day>
            Dude!
          </day>
        </summer>: Node
      )

      (Text("   "): Node) should equal(Text("   "))

      (<div>{ Text("My name is ") }{ Text("Harry") }</div>: Node) should equal(<div>My name is Harry</div>)
      (<summer><day>{ Text("Hello ") }{ Text("Dude!") }</day></summer>: Node) should equal(<summer><day>Hello Dude!</day></summer>)
      (<div>My name is { PCData("Harry") }</div>: Node) should equal(<div>My name is Harry</div>)
    }
  }

  describe("Streamlined Xml Normalization of NodeSeq") {

    it("should leave already-normalized XML alone") {

      (<summer></summer>: NodeSeq) should equal(<summer></summer>)

      (Text("Bla"): NodeSeq) should equal(Text("Bla"))
    }

    it("should zap text that is only whitespace, unless it is already a Text") {

      (<summer> </summer>: NodeSeq) should equal(<summer></summer>)

      (<summer>
       </summer>: NodeSeq) should equal(<summer></summer>)

      (<summer>
         <day></day>
       </summer>: NodeSeq) should equal(<summer><day></day></summer>)

      <summer><day></day></summer> should equal(
        <summer>
          <day></day>
        </summer>: NodeSeq
      )

      <summer><day>Dude!</day></summer> should equal(
        <summer>
          <day>
            Dude!
          </day>
        </summer>: NodeSeq
      )

      (Text("   "): NodeSeq) should equal(Text("   "))

      (<div>{ Text("My name is ") }{ Text("Harry") }</div>: NodeSeq) should equal(<div>My name is Harry</div>)
      (<summer><day>{ Text("Hello ") }{ Text("Dude!") }</day></summer>: NodeSeq) should equal(<summer><day>Hello Dude!</day></summer>)
      (<div>My name is { PCData("Harry") }</div>: NodeSeq) should equal(<div>My name is Harry</div>)
    }
  }
}
