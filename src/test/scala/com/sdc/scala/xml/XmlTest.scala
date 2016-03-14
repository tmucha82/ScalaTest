package com.sdc.scala.xml

import org.scalatest.FunSuite

import scala.xml.NodeSeq

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

    val therm = new CCTherm {
      val description = "hot dog #5"
      val yearMade = 1952
      val dateObtained = "March 14, 2006"
      val bookPrice = 2199
      val purchasePrice = 500
      val condition = 9
    }
    assert("<cctherm><description>hot dog #5</description><yearMade>1952</yearMade><dateObtained>March 14, 2006</dateObtained><bookPrice>2199</bookPrice><purchasePrice>500</purchasePrice><condition>9</condition></cctherm>" === therm.toXML.toString)

  }

}
