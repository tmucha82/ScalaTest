package com.sdc.scala.xml

import java.io.File

import org.scalatest.FunSuite

import scala.io.Source
import scala.xml.{NodeSeq, XML}

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
/*
    object Dupa extends Enumeration {
      val A = Value
      val B = Value
      val Other = Value
    }

    def proc(node: scala.xml.Node): Value =
      node match {
        case <a>{contents}</a> => "It's an a: " + contents
        case <b>{contents}</b> => "It's a b: " + contents
        case _ => "It's something else."
      }
*/
  }
}
