package com.sdc.scala.spreadsheet

import scala.swing.{ListView, ScrollPane, Table}

class Spreadsheet(val height: Int, val width: Int) extends ScrollPane {

  import cellModel._
  val cellModel = new Model(height, width)

  val table = new Table(height, width) {
    rowHeight = 25
    autoResizeMode = Table.AutoResizeMode.Off
    showGrid = true
    gridColor = new java.awt.Color(150, 150, 150)
  }

  val rowHeader =
    new ListView((0 until height) map (_.toString)) {
      fixedCellWidth = 30
      fixedCellHeight = table.rowHeight
    }

  viewportView = table
  rowHeaderView = rowHeader
}