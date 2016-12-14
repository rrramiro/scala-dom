package fr.ramiro.scala.dom

import org.scalatest.{FunSpec, Matchers}
import scala.xml.NodeSeq

class StreamlinedXmlNormMethodsSpec extends FunSpec with Matchers with StreamlinedXmlNormMethods {
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
