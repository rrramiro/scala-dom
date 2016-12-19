package fr.ramiro.scala.dom

import java.io.StringReader

import org.junit.Assert
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.xml.EntityRef

@RunWith(classOf[JUnitRunner])
class XmlTest extends FunSuite {

  test("attributes") {
    val p = <foo>
              <bar gt='ga' value="3"/>
              <baz bazValue="8"/>
              <bar value="5" gi='go'/>
            </foo>.asW3cNode()
    val pelems_1 = for (x <- p \ "bar"; y <- p \ "baz") yield {
      (x \ "@value").text + (y \ "@bazValue").text + "!"
    }

    val pelems_2 = List("38!", "58!")
    assert(pelems_1 === pelems_2)
    assert("8" === (p \\ "@bazValue").text)
  }

  test("utf8") {
    val parsedxmlA = convertStringToNode("<personne id='p0003' nom='&#x015e;ahingz' />")
    val c = (parsedxmlA \ "@nom").text.charAt(0)
    assert(c === '\u015e')
  }

  test("queryBooks") {

    val books =
      <bks>
        <book><title>Blabla</title></book>
        <book><title>Blubabla</title></book>
        <book><title>Baaaaaaalabla</title></book>
      </bks>.asW3cNode()

    val reviews =
      <reviews>
        <entry>
          <title>Blabla</title>
          <remarks>
            Hallo Welt.
          </remarks>
        </entry>
        <entry>
          <title>Blubabla</title>
          <remarks>
            Hello Blu
          </remarks>
        </entry>
        <entry>
          <title>Blubabla</title>
          <remarks>
            rem 2
          </remarks>
        </entry>
      </reviews>.asW3cNode()

    val results1 = new scala.xml.PrettyPrinter(80, 5).formatNodes(
      for (
        t <- books \\ "title";
        r <- reviews \\ "entry" if (r \ "title") xml_== t
      ) yield <result>
                { t.asScala }
                { (r \ "remarks").asScala }
              </result>
    )

    val res1 = <result>
                 <title>Blabla</title>
                 <remarks> Hallo Welt. </remarks>
               </result><result>
                          <title>Blubabla</title>
                          <remarks> Hello Blu </remarks>
                        </result><result>
                                   <title>Blubabla</title>
                                   <remarks> rem 2 </remarks>
                                 </result>
    val results1Expected = new scala.xml.PrettyPrinter(80, 5).formatNodes(res1)
    assert(results1Expected === results1)
  }

  test("queryPhoneBook") {
    val phoneBook =
      <phonebook>
        <descr>
          This is the<b>phonebook</b>
          of the
          <a href="http://acme.org">ACME</a>
          corporation.
        </descr>
        <entry>
          <name>John</name>
          <phone where="work">  +41 21 693 68 67</phone>
          <phone where="mobile">+41 79 602 23 23</phone>
        </entry>
      </phonebook>.asW3cNode()

    val addrBook =
      <addrbook>
        <descr>
          This is the<b>addressbook</b>
          of the
          <a href="http://acme.org">ACME</a>
          corporation.
        </descr>
        <entry>
          <name>John</name>
          <street> Elm Street</street>
          <city>Dolphin City</city>
        </entry>
      </addrbook>.asW3cNode()

    val actual: String = new scala.xml.PrettyPrinter(80, 5).formatNodes(
      for (
        t <- addrBook \\ "entry";
        r <- phoneBook \\ "entry" if (t \ "name") xml_== (r \ "name")
      ) yield <result>
                { t.child.map { _.asScala } }
                { (r \ "phone").asScala }
              </result>
    )
    val expected = new scala.xml.PrettyPrinter(80, 5).formatNodes(
      <result>
        <name>John</name>
        <street> Elm Street</street>
        <city>Dolphin City</city>
        <phone where="work"> +41 21 693 68 67</phone>
        <phone where="mobile">+41 79 602 23 23</phone>
      </result>
    )
    assert(expected === actual)
  }

  test("namespaces") {
    val cuckoo = <cuckoo xmlns="http://cuckoo.com"><foo/><bar/></cuckoo>.asW3cNode(true);

    assert("http://cuckoo.com" === cuckoo.namespace)
    for (n <- cuckoo \ "_") {
      assert("http://cuckoo.com" === n.namespace)
    }
  }

  test("namespacesWithNestedXmls") {
    val foo = <f:foo xmlns:f="fooUrl"></f:foo>;
    val bar = <b:bar xmlns:b="barUrl">{ foo }</b:bar>.asW3cNode(true);
    val expected = """<b:bar xmlns:b="barUrl"><f:foo xmlns:f="fooUrl"/></b:bar>"""
    val actual = bar.mkString
    assert(expected === actual)
  }

  val e: scala.xml.MetaData = scala.xml.Null //Node.NoAttributes
  val sc: scala.xml.NamespaceBinding = scala.xml.TopScope
  def Elem(prefix: String, label: String, attributes: scala.xml.MetaData, scope: scala.xml.NamespaceBinding, child: scala.xml.Node*): scala.xml.Elem =
    scala.xml.Elem.apply(prefix, label, attributes, scope, minimizeEmpty = true, child: _*)

  lazy val parsedxml1 = scala.xml.XML.load(new scala.xml.InputSource(new StringReader("<hello><world/></hello>"))).asW3cNode()
  lazy val parsedxml11 = scala.xml.XML.load(new scala.xml.InputSource(new StringReader("<hello><world/></hello>"))).asW3cNode()
  val xmlFile2 = "<bib><book><author>Peter Buneman</author><author>Dan Suciu</author><title>Data on ze web</title></book><book><author>John Mitchell</author><title>Foundations of Programming Languages</title></book></bib>";
  lazy val parsedxml2 = scala.xml.XML.load(new scala.xml.InputSource(new StringReader(xmlFile2))).asW3cNode()

  test("equality") {
    val c = new scala.xml.Node {
      def label = "hello"
      override def hashCode() =
        scala.xml.Utility.hashCode(prefix, label, attributes.hashCode(), scope.hashCode(), child);
      def child = scala.xml.Elem(null, "world", e, sc);
      //def attributes = e;
      override def text = ""
    }.asW3cNode()

    assert(c xml_== parsedxml11)
    assert(parsedxml1 xml_== parsedxml11)
    //    assert(List(parsedxml1) xml_sameElements List(parsedxml11))
    //    assert(Array(parsedxml1).toList xml_sameElements List(parsedxml11))

    val x2 = "<book><author>Peter Buneman</author><author>Dan Suciu</author><title>Data on ze web</title></book>";

    val i = new scala.xml.InputSource(new StringReader(x2))
    val x2p = scala.xml.XML.load(i).asW3cNode()

    assert(x2p xml_== Elem(null, "book", e, sc,
      Elem(null, "author", e, sc, scala.xml.Text("Peter Buneman")),
      Elem(null, "author", e, sc, scala.xml.Text("Dan Suciu")),
      Elem(null, "title", e, sc, scala.xml.Text("Data on ze web"))).asW3cNode())
  }

  test("unparsed") {
    val xmlAttrValueNorm = "<personne id='p0003' nom='&#x015e;ahingz' />";
    {
      val isrcA = new scala.xml.InputSource(new StringReader(xmlAttrValueNorm))
      val parsedxmlA = scala.xml.XML.load(isrcA).asW3cNode()
      val c = (parsedxmlA \ "@nom").text.charAt(0)
      assert(c === '\u015e')
    }
    {
      val isr = scala.io.Source.fromString(xmlAttrValueNorm)
      val pxmlB = scala.xml.parsing.ConstructingParser.fromSource(isr, false)
      val parsedxmlB = W3cNodeSeq(pxmlB.element(scala.xml.TopScope).map { _.asW3cNode() })
      val c = (parsedxmlB \ "@nom").text.charAt(0)
      assert(c === '\u015e')
    }

    val p = scala.xml.parsing.ConstructingParser.fromSource(scala.io.Source.fromString("<foo bar:attr='&amp;'/>"), true)
    val n = p.element(new scala.xml.NamespaceBinding("bar", "BAR", scala.xml.TopScope))(0).asW3cNode()
    //assert(n.attributes.get("BAR", n, "attr").nonEmpty)
  }

  test("dodgyNamespace") {
    val x = <flog xmlns:ee="http://ee.com"><foo xmlns:dog="http://dog.com"><dog:cat/></foo></flog>.asW3cNode()
    assert(x.mkString.matches(".*xmlns:dog=\"http://dog.com\".*"))
  }

  val ax = <hello foo="bar" x:foo="baz" xmlns:x="the namespace from outer space">
             <world/>
           </hello>.asW3cNode()

  val cx = <z:hello foo="bar" xmlns:z="z" x:foo="baz" xmlns:x="the namespace from outer space">
             crazy text world
           </z:hello>.asW3cNode()

  val bx = <hello foo="bar&amp;x"></hello>.asW3cNode()

  test("XmlEx") {
    assert("""<hello xmlns:x="the namespace from outer space" foo="bar" x:foo="baz">
             |             <world/>
             |           </hello>""".stripMargin === ax.mkString)
    assert("<hello foo=\"bar&amp;x\"/>" === bx.mkString)
    assert("""<z:hello xmlns:z="z" xmlns:x="the namespace from outer space" foo="bar" x:foo="baz">
             |             crazy text world
             |           </z:hello>""".stripMargin === cx.mkString)

    assert((ax \ "@foo") xml_== "bar")
    assert((ax \ "@foo") xml_== scala.xml.Text("bar").asW3cNode())
    assert((bx \ "@foo") xml_== "bar&x") // dto.
    assert((bx \ "@foo") xml_== scala.xml.Text("bar&x").asW3cNode())
    assert((bx \ "@foo").xml_sameElements(List(xml.Text("bar&x"))))
  }

  ignore("XmlEy") {
    val z = ax \ "@{the namespace from outer space}foo"
    assert((ax \ "@{the namespace from outer space}foo") xml_== "baz")
    assert((cx \ "@{the namespace from outer space}foo") xml_== "baz")

    intercept[IllegalArgumentException] {
      ax \ "@"
    }
    intercept[IllegalArgumentException] {
      ax \ "@{"
    }
    intercept[IllegalArgumentException] {
      ax \ "@{}"
    }
  }

  test("comment") {
    assert("<!-- thissa comment -->" === <!-- thissa comment -->.asW3cNode().mkString)
  }

  test("weirdElem") {
    assert("<?this is a pi foo bar = && {{ ?>" === scala.xml.ProcInstr("this", "is a pi foo bar = && {{ ").asW3cNode().mkString)
  }

  ignore("escape") {
    val actual = <![CDATA[
 "Come, come again, whoever you are, come!
Heathen, fire worshipper or idolatrous, come!
Come even if you broke your penitence a hundred times,
Ours is the portal of hope, come as you are."
                              Mevlana Celaleddin Rumi]]>.asW3cNode()

    assert("""
| &quot;Come, come again, whoever you are, come!
|Heathen, fire worshipper or idolatrous, come!
|Come even if you broke your penitence a hundred times,
|Ours is the portal of hope, come as you are.&quot;
|                              Mevlana Celaleddin Rumi""".stripMargin === actual.mkString)
  }

  ignore("unparsed2") {
    object myBreak extends scala.xml.Unparsed("<br />")
    assert("<foo><br /></foo>" === <foo>{ myBreak }</foo>.asW3cNode().mkString) // shows use of unparsed
  }

  test("cleanProcInst") {
    val input = convertStringToNode(
      """
        |<addrbook>
        |    <?ed_insert_start author="Editor" timestamp="20150513T163416+0100"?>
        |    <descr>
        |      This is the<b>addressbook</b>
        |      of the
        |      <a href="http://acme.org">ACME</a>
        |      corporation.
        |    </descr>
        |    <!-- Some comments -->
        |    <?ed_insert_end?>
        |    <?ed_insert_start author="Editor" timestamp="20150129T103855+0000"?>
        |    <entry>
        |      <name>John</name>
        |      <street> Elm Street</street>
        |      <city>Dolphin City</city>
        |    </entry>
        |    <?ed_insert_end?>
        |</addrbook><?ed_options track_changes="on"?>
      """.stripMargin
    )

    input.removeWithXPath("//processing-instruction()")

    val actual = new scala.xml.PrettyPrinter(80, 5).formatNodes(input.asScala)
    val expected = new scala.xml.PrettyPrinter(80, 5).formatNodes(
      <addrbook>
        <descr>
          This is the<b>addressbook</b>
          of the
          <a href="http://acme.org">ACME</a>
          corporation.
        </descr>
        <entry>
          <name>John</name>
          <street> Elm Street</street>
          <city>Dolphin City</city>
        </entry>
      </addrbook>
    )
    assert(actual === expected)
  }

  test("streamline") {
    val phoneBook =
      <phonebook>
        <descr>
          This is the<b>phonebook</b>
          of the
          <a href="http://acme.org">ACME</a>
          corporation.
        </descr>
        <entry>
          <name>John</name>
          <phone where="work">  +41 21 693 68 67</phone>
          <phone where="mobile">+41 79 602 23 23</phone>
        </entry>
      </phonebook>.asW3cNode()

    val expected = """<phonebook> <descr> This is the<b>phonebook</b> of the <a href="http://acme.org">ACME</a> corporation. </descr> <entry> <name>John</name> <phone where="work"> +41 21 693 68 67</phone> <phone where="mobile">+41 79 602 23 23</phone> </entry> </phonebook>"""
    phoneBook.streamLined()
    val actual = phoneBook.asScala.mkString
    assert(actual === expected)
  }

  test("mkString") {
    val phoneBook =
      <phonebook>
        <descr>
          This is the<b>phonebook</b>
          of the
          <a href="http://acme.org">ACME</a>
          corporation.
        </descr>
        <entry>
          <name>John</name>
          <phone where="work">  +41 21 693 68 67</phone>
          <phone where="mobile">+41 79 602 23 23</phone>
        </entry>
      </phonebook>.asW3cNode()
    phoneBook.streamLined()
    val actual = phoneBook.child.mkString
    assert("phonebook" === phoneBook.label)
    assert(actual === """ <descr> This is the<b>phonebook</b> of the <a href="http://acme.org">ACME</a> corporation. </descr> <entry> <name>John</name> <phone where="work"> +41 21 693 68 67</phone> <phone where="mobile">+41 79 602 23 23</phone> </entry> """)
  }

  test("delete attributes with ns") {
    val bookXml = <phonebook ed:global="PHP78558_eng" xmlns:ed="www.editor.com" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                    <xref type="contact" idref="00001" ed:cross-reference="00001"/>
                  </phonebook>
    val book = fr.ramiro.scala.dom.convertStringToNode(bookXml.mkString, namespaceAware = true)
    book.removeWithXPath("//@ed:*", Map("ed" -> "www.editor.com"))
    book.streamLined()
    val actual = book.asScala.mkString
    val expected = """<phonebook> <xref type="contact" idref="00001"/> </phonebook>"""
    assert(actual === expected)
  }

  test("attributeOperator") {
    val xml = <foo bar="apple"/>.asW3cNode()
    assert("apple" === xml \@ "bar")
  }

  test("attributePathRootNoAttribute") {
    val xml = <foo/>.asW3cNode()
    assert(W3cNodeSeq.empty === xml \ "@bar")
  }

  test("attributePathIllegalEmptyAttribute") {
    intercept[IllegalArgumentException] {
      val xml = <foo/>.asW3cNode()
      xml \ "@"
    }
  }

  test("attributePathDescendantAttributes") {
    val xml = <a><b bar="1"/><b bar="2"/></a>.asW3cNode()
    assert(Seq("1", "2") === (xml \\ "@bar").map { _.text })
  }

  test("attributeDescendantPathChildAttributes") {
    val xml = <a><b bar="1"/><b bar="2"/></a>.asW3cNode()
    assert(Seq("1", "2") === (xml \ "b" \\ "@bar").map { _.text })
  }

  test("attributeDescendantPathDescendantAttributes") {
    val xml = <a><b bar="1"/><b bar="2"/></a>.asW3cNode()
    assert(Seq("1", "2") === (xml \\ "b" \\ "@bar").map { _.text })
  }

  test("attributeChildDescendantPathDescendantAttributes") {
    val xml = <x><a><b bar="1"/><b bar="2"/></a></x>.asW3cNode()
    assert(Seq("1", "2") === (xml \ "a" \\ "@bar").map { _.text })
  }

  test("attributeDescendantDescendantPathDescendantAttributes") {
    val xml = <x><a><b bar="1"/><b bar="2"/></a></x>.asW3cNode()
    assert(Seq("1", "2") === (xml \\ "b" \\ "@bar").map { _.text })
  }

  test("attributePathDescendantIllegalEmptyAttribute") {
    intercept[IllegalArgumentException] {
      val xml = <foo/>.asW3cNode()
      xml \\ "@"
    }
  }

  test("attributePathNoDescendantAttributes") {
    val xml = <a><b bar="1"/><b bar="2"/></a>.asW3cNode()
    assert(W3cNodeSeq.empty === (xml \\ "@oops"))
  }

  test("attributePathOneChildWithAttributes") {
    val xml = <a><b bar="1"/>></a>.asW3cNode()
    assert("1" === (xml \ "b" \ "@bar").text)
  }

  test("attributePathTwoChildrenWithAttributes") {
    val xml = <a><b bar="1"/><b bar="2"/></a>.asW3cNode()
    val b = xml \ "b"
    assert(2 === b.length)
    val barFail = b \ "@bar"
    val barList = b.map(_ \ "@bar")
    //assert(W3cNodeSeq.empty === barFail)
    assert(Seq("1", "2") === barFail.map { _.text })
    assert(Seq("1", "2") === barList.map { _.text })
  }

  test("if \\ brings only child nodes") {
    val phoneBook =
      <phonebook>
        <descr>
          This is the<b>phonebook</b>
          of the
          <a href="http://acme.org">ACME</a>
          corporation.
        </descr>
        <entry>
          <name>John</name>
          <phone where="work">  +41 21 693 68 67</phone>
          <phone where="mobile">+41 79 602 23 23</phone>
        </entry>
      </phonebook>.asW3cNode()
    val actual = phoneBook \ "phone"
    assert(actual.size === 0)
  }

  test("Forget namespaces when copyNodes") {
    val xml =
      <phonebook id="1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../phonebook.xsd">
        <descr>
          This is the<b>phonebook</b>
          of the
          <a href="http://acme.org">ACME</a>
          corporation.
        </descr>
        <entry>
          <name>John</name>
          <phone where="work">  +41 21 693 68 67</phone>
          <phone where="mobile">+41 79 602 23 23</phone>
          <description>
            <p>
              Contact type:<i>Work</i>
            </p>
          </description>
        </entry>
      </phonebook>

    val xmlInput = convertStringToNode(xml.mkString, namespaceAware = true)

    assert(!(xmlInput \ "entry" \ "description").copyNodes(false).child.mkString.contains("xmlns"))
  }

  test("Forget namespaces with copyNodes 2") {
    val xml =
      <phonebook xmlns:ed="www.editor.com" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" editable="false" id="1" xsi:noNamespaceSchemaLocation="phonebook.xsd">
        <descr>
          <p>
            This is the<b>phonebook</b>
            of the
            <a href="http://acme.org">ACME</a>
            corporation.
          </p>
        </descr>
        <entry>
          <name>John</name>
          <phone where="work">  +41 21 693 68 67</phone>
          <phone where="mobile">+41 79 602 23 23</phone>
          <description>
            <p>
              Contact type:<i>Work</i>
            </p>
          </description>
        </entry>
      </phonebook>
    val xmlInput = convertStringToNode(xml.mkString, namespaceAware = true)

    assert((xmlInput \ "descr").child.mkString.contains("xmlns"))
    val xmlInputCopy = xmlInput.copyNodes(false)
    xmlInputCopy.removeNamespaceAttributes()
    assert(!(xmlInputCopy \ "descr").child.mkString.contains("xmlns"))
  }

  test("Forget namespaces with copyNodes 3") {
    val xml =
      <result id="A" timestamp="1449172308147">
        <phonebook xmlns:ed="www.editor.com" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" editable="false" id="1" xsi:noNamespaceSchemaLocation="phonebook.xsd">
          <descr>
            <p>
              This is the<b>phonebook</b>
              of the
              <a href="http://acme.org">ACME</a>
              corporation.
            </p>
          </descr>
          <entry>
            <name>John</name>
            <phone where="work">  +41 21 693 68 67</phone>
            <phone where="mobile">+41 79 602 23 23</phone>
            <description>
              <p>
                Contact type:<i>Work</i>
              </p>
            </description>
          </entry>
        </phonebook>
      </result>
    val xmlInput = convertStringToNode(xml.mkString, namespaceAware = true)

    assert(xmlInput.mkString.contains("xmlns"))
    val xmlInputCopy = xmlInput.copyNodes(false)
    xmlInputCopy.removeNamespaceAttributes()
    assert(!xmlInputCopy.mkString.contains("xmlns"))
  }

  test("copy nodes") {
    val xml = <a><b bar="1"/><c bar="2"/></a>.asW3cNode()
    val xmlCopy: W3cNodeSeq = xml.copyNodes()
    xml.removeWithXPath("""//c""")
    xmlCopy.removeWithXPath("""//b""")
    assert(xml.mkString === """<a><b bar="1"/></a>""")
    assert(xmlCopy.mkString === """<a><c bar="2"/></a>""")
  }

  test("conversion") {
    val phoneBook =
      <phonebook>
        <descr>
          This is the
          <b>phonebook</b>
          of the
          <a href="http://acme.org">ACME</a>
          corporation.
          { EntityRef("lt") }
        </descr>
        <!-- some comments-->
        <entry>
          <name>John</name>
          <phone where="work">+41 21 693 68 67</phone>
          <phone where="mobile">+41 79 602 23 23</phone>
        </entry><?xhiveTest type="phone"?>
      </phonebook>

    val converter = convertScalaNodeToNode(_: scala.xml.Node)
    val actual = converter(phoneBook)
    val actualChild = new W3cNodeSeq(phoneBook.child.map { converter })
    Assert.assertEquals(phoneBook.mkString, actual.mkString)
    Assert.assertEquals(phoneBook.child.mkString, actualChild.map { _.mkString }.mkString)
  }

  test("wildcard") {
    val people =
      <people>
        <family>
          <person>Mom</person>
        </family>
        <friends>
          <person>Bill</person>
          <person>Candy</person>
        </friends>
      </people>.asW3cNode()

    val allPeople = (people \ "_" \ "person").map { _.text }
    Assert.assertEquals(3, allPeople.length)
    assert(allPeople.contains("Mom"))
    assert(allPeople.contains("Bill"))
    assert(allPeople.contains("Candy"))
  }

  test("findNodeByXpath") {
    val phoneBook =
      <phonebook>
        <descr>
          This is the<b>phonebook</b>
          of the
          <a href="http://acme.org">ACME</a>
          corporation.
        </descr>
        <entry>
          <name>John</name>
          <phone where="work">  +41 21 693 68 67</phone>
          <phone where="mobile">+41 79 602 23 23</phone>
        </entry>
      </phonebook>.asW3cNode()
    val actual = phoneBook.findNodeByXpath(".//phone[@where='work']")
    assert(actual.isDefined)
    assert(actual.get.text === "  +41 21 693 68 67")
    assert(phoneBook.findNodeByXpath(".//phone[@where='home']").isEmpty)
  }

  test("xpath") {
    val phoneBook = <phonebook>
                      <entry outputclass="group1 group2">Jhon</entry>
                      <entry outputclass="group2 group3">Joe</entry>
                      <entry outputclass="group3">Jane</entry>
                    </phonebook>.asW3cNode()

    def findByOutputClass(outputclassesFilter: String*) = {
      val name = "entry"
      val xpath = if (outputclassesFilter.nonEmpty) outputclassesFilter.mkString(s"./$name[contains(@outputclass,'", "') or contains(@outputclass,'", "')]") else s"./$name"
      phoneBook.findNodesByXpath(xpath)
    }

    assert(findByOutputClass().size === 3)
    assert(findByOutputClass("group0").size === 0)
    assert(findByOutputClass("group1").size === 1)
    assert(findByOutputClass("group2").size === 2)
    assert(findByOutputClass("group2", "group1").size === 2)
    assert(findByOutputClass("group3").size === 2)

  }
}
