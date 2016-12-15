package fr.ramiro.scala.dom

import org.scalatest.{ FunSpec, Matchers }
import scala.xml.{ Node, Text, NodeSeq, PCData }

class StreamlinedXmlNormMethodsSpec extends FunSpec with Matchers with StreamlinedXmlNormMethods {

  describe("Xml Normalization of Elems") {

    it("should leave already-normalized XML alone") {
      <summer></summer>.norm == <summer></summer> shouldBe true
    }

    it("should zap text that is only whitespace") {
      <summer> </summer>.norm == <summer></summer> shouldBe true
      <summer>
      </summer>.norm == <summer></summer> shouldBe true
      <summer>
        <day></day>
      </summer>.norm == <summer><day></day></summer> shouldBe true
      <summer><day></day></summer> ==
        <summer>
          <day></day>
        </summer>.norm shouldBe true
      <summer><day>Dude!</day></summer> ==
        <summer>
          <day>
            Dude!
          </day>
        </summer>.norm shouldBe true
      <div>{ Text("My name is ") }{ Text("Harry") }</div>.norm shouldBe <div>My name is Harry</div>
      (<summer><day>{ Text("Hello ") }{ Text("Dude!") }</day></summer>).norm shouldBe <summer><day>Hello Dude!</day></summer>
      (<div>My name is { PCData("Harry") }</div>).norm shouldBe <div>My name is Harry</div>
    }
  }

  describe("Xml Normalization of Nodes") {

    it("should leave already-normalized XML alone") {
      (<summer></summer>: Node).norm == <summer></summer> shouldBe true
      (Text("Bla"): Node).norm shouldBe Text("Bla")
    }

    it("should zap text that is only whitespace, unless it is already a Text") {
      (<summer> </summer>.norm: Node) == <summer></summer> shouldBe true
      (<summer>
       </summer>.norm: Node) == <summer></summer> shouldBe true
      (<summer>
         <day></day>
       </summer>.norm: Node) == <summer><day></day></summer> shouldBe true
      <summer><day></day></summer> ==
        (<summer>
           <day></day>
         </summer>.norm: Node) shouldBe true
      <summer><day>Dude!</day></summer> ==
        (<summer>
           <day>
             Dude!
           </day>
         </summer>.norm: Node) shouldBe true
      (Text("   "): Node).norm shouldBe Text("   ")
      (<div>{ Text("My name is ") }{ Text("Harry") }</div>: Node).norm shouldBe <div>My name is Harry</div>
      (<summer><day>{ Text("Hello ") }{ Text("Dude!") }</day></summer>: Node).norm shouldBe <summer><day>Hello Dude!</day></summer>
      (<div>My name is { PCData("Harry") }</div>: Node).norm shouldBe <div>My name is Harry</div>
    }
  }

  describe("Xml Normalization of NodeSeq") {

    it("should leave already-normalized XML alone") {
      (<summer></summer>: NodeSeq).norm == <summer></summer> shouldBe true
      (Text("Bla"): NodeSeq).norm shouldBe Text("Bla")
    }

    it("should zap text that is only whitespace, unless it is already a Text") {
      (<summer> </summer>.norm: NodeSeq) == <summer></summer> shouldBe true
      (<summer>
       </summer>.norm: NodeSeq) == <summer></summer> shouldBe true
      (<summer>
         <day></day>
       </summer>.norm: NodeSeq) == <summer><day></day></summer> shouldBe true
      <summer><day></day></summer> ==
        (<summer>
           <day></day>
         </summer>.norm: NodeSeq) shouldBe true
      <summer><day>Dude!</day></summer> ==
        (<summer>
           <day>
             Dude!
           </day>
         </summer>.norm: NodeSeq) shouldBe true
      (Text("   "): NodeSeq).norm shouldBe Text("   ")
      (<div>{ Text("My name is ") }{ Text("Harry") }</div>: NodeSeq).norm shouldBe <div>My name is Harry</div>
      (<summer><day>{ Text("Hello ") }{ Text("Dude!") }</day></summer>: Node).norm shouldBe <summer><day>Hello Dude!</day></summer>
      (<div>My name is { PCData("Harry") }</div>: NodeSeq).norm shouldBe <div>My name is Harry</div>
    }

    it("should keep the order of the nodes while merging adjacent Text") {
      (<seasons>
         <spring>
           <day>One</day>
           <day>Tow</day>
         </spring>
         <summer>
           <day>Three</day>
         </summer>
         <fall>
           <day>Four</day>
         </fall>
         <winter>
           <day>Five</day>
           <day>Six</day>
           <day>Seven</day>
         </winter>
       </seasons>: NodeSeq).norm shouldBe <seasons><spring><day>One</day><day>Tow</day></spring><summer><day>Three</day></summer><fall><day>Four</day></fall><winter><day>Five</day><day>Six</day><day>Seven</day></winter></seasons>
    }
  }
}
