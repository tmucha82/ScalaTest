package com.sdc.scala.xml

abstract class CCTherm {
  val description: String
  val yearMade: Int
  val dateObtained: String
  val bookPrice: Int      // in US cents
  val purchasePrice: Int  // in US cents
  val condition: Int      // 1 to 10

  override def toString = description

  def toXML = <cctherm><description>{description}</description><yearMade>{yearMade}</yearMade><dateObtained>{dateObtained}</dateObtained><bookPrice>{bookPrice}</bookPrice><purchasePrice>{purchasePrice}</purchasePrice><condition>{condition}</condition></cctherm>
}