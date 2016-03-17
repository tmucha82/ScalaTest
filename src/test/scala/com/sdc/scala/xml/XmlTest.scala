package com.sdc.scala.xml

import java.io.File

import org.scalatest.FunSuite

import scala.io.Source
import scala.xml.{Node, NodeSeq, XML}

class XmlTest extends FunSuite {
  test("XML literals") {
    var xml = <a>{"hello" + ", world"}</a>
    assert("<a>hello, world</a>" === xml.toString)

    val yearMade = 1955
    xml = <a>{if (yearMade < 2000)
      <old>{yearMade}</old>
    else NodeSeq.Empty}</a>
    assert("<a><old>1955</old></a>" === xml.toString)

    xml = <a>{"</a>pizza<a>"}</a>
    assert("<a>&lt;/a&gt;pizza&lt;a&gt;</a>" === xml.toString)
  }

  test("ccTherm") {
    var therm = new CCTherm {
      val description = "hot dog #5"
      val yearMade = 1952
      val dateObtained = "March 14, 2006"
      val bookPrice = 2199
      val purchasePrice = 500
      val condition = 9
    }
    assert("<cctherm><description>hot dog #5</description><yearMade>1952</yearMade><dateObtained>March 14, 2006</dateObtained><bookPrice>2199</bookPrice><purchasePrice>500</purchasePrice><condition>9</condition></cctherm>" === therm.toXML.toString)

    val toDeserialize = <cctherm><description>hot dog #5</description><yearMade>1952</yearMade><dateObtained>March 14, 2006</dateObtained><bookPrice>2199</bookPrice><purchasePrice>500</purchasePrice><condition>9</condition></cctherm>
    therm = therm.fromXML(toDeserialize)
    assert("hot dog #5" === therm.description)
    assert(1952 === therm.yearMade)
    assert("March 14, 2006" === therm.dateObtained)
    assert(2199 === therm.bookPrice)
    assert(500 === therm.purchasePrice)
    assert(9 === therm.condition)

    var source:Source = null
    val fileName = "./therm1.xml"
    val file = new File(fileName)
    try {
      XML.save(fileName, therm.toXML, "UTF-8", xmlDecl = true, null)
      assert(file.exists())
      source = Source.fromFile(fileName)
      assert("<?xml version='1.0' encoding='UTF-8'?><cctherm><description>hot dog #5</description><yearMade>1952</yearMade><dateObtained>March 14, 2006</dateObtained><bookPrice>2199</bookPrice><purchasePrice>500</purchasePrice><condition>9</condition></cctherm>" === source.getLines().toList.mkString)

      val loadedXml = XML.loadFile(fileName)
      assert("<cctherm><description>hot dog #5</description><yearMade>1952</yearMade><dateObtained>March 14, 2006</dateObtained><bookPrice>2199</bookPrice><purchasePrice>500</purchasePrice><condition>9</condition></cctherm>" === loadedXml.toString)
    } finally {
      if(source != null) source.close()

      if(file.exists()) {
        assert(file.delete())
      }
    }
  }

  test("escape of { char") {
    assert("<a>{{brace yourself!}}</a>" === <a>{{{{brace yourself!}}}}</a>.toString)
  }

  test("taking XML apart") {
    assert("Sounds good" === <a>Sounds <tag/>good</a>.text)
    assert("input ---> output" === <a>input ---&gt; output</a>.text)
    assert("<b><c>hello</c></b>" === (<a><b><c>hello</c></b></a> \ "b").toString())
    assert("" === (<a><b><c>hello</c></b></a> \ "c").toString())
    assert("<c>hello</c>" === (<a><b><c>hello</c></b></a> \\ "c").toString())
    assert("" === (<a><b><c>hello</c></b></a> \ "a").toString())
    assert("<a><b><c>hello</c></b></a>" === (<a><b><c>hello</c></b></a> \\ "a").toString())
  }

  test("extracting attributes") {
    val joe = <employee name="Joe" rank="code monkey" serial="123"/>
    assert("<employee name=\"Joe\" rank=\"code monkey\" serial=\"123\"/>" === joe.toString())
    assert("Joe" === (joe \ "@name").toString())
    assert("123" === (joe \ "@serial").toString())
  }

  test("xml pattern matching") {
    object Element extends Enumeration {
      val A = Value
      val B = Value
      val Other = Value
    }

    def process(node: Node): Element.Value =
      node match {
        case <a>{contents}</a> => Element.A
        case <b>{contents}</b> => Element.B
        case _ => Element.Other
      }

    assert(Element.A === process(<a>apple</a>))
    assert(Element.B === process(<b>banana</b>))
    assert(Element.Other === process(<c>cherry</c>))
    assert(Element.Other === process(<a>a <em>red</em> apple</a>))
    assert(Element.Other === process(<a/>))

    def process2(node: Node): Element.Value =
      node match {
        case <a>{contents @ _*}</a> => Element.A
        case <b>{contents @ _*}</b> => Element.B
        case _ => Element.Other
      }

    assert(Element.A === process2(<a>apple</a>))
    assert(Element.B === process2(<b>banana</b>))
    assert(Element.Other === process2(<c>cherry</c>))
    assert(Element.A === process2(<a>a <em>red</em> apple</a>))
    assert(Element.A === process2(<a/>))


    val catalog =
      <catalog>
        <cctherm>
          <description>hot dog #5</description>
          <yearMade>1952</yearMade>
          <dateObtained>March 14, 2006</dateObtained>
          <bookPrice>2199</bookPrice>
          <purchasePrice>500</purchasePrice>
          <condition>9</condition>
        </cctherm>
        <cctherm>
          <description>Sprite Boy</description>
          <yearMade>1964</yearMade>
          <dateObtained>April 28, 2003</dateObtained>
          <bookPrice>1695</bookPrice>
          <purchasePrice>595</purchasePrice>
          <condition>5</condition>
        </cctherm>
      </catalog>

    val descriptions = catalog match {
      case <catalog>{therms @ _*}</catalog> => for (therm @ <cctherm>{_*}</cctherm> <- therms) yield (therm \ "description").text
    }

    assert(List("hot dog #5", "Sprite Boy") === descriptions)
  }
}
