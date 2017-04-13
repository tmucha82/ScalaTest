package com.sdc.scala.spreadsheet

import scala.swing.{MainFrame, SimpleSwingApplication}

object Main extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "ScalaSheet"
    contents = new Spreadsheet(100, 26)
  }
}
