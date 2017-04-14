package com.sdc.scala.spreadsheet

import scala.swing._
import scala.swing.event.{EditDone, ValueChanged, TableUpdated}

class Spreadsheet(val height: Int, val width: Int) extends ScrollPane {

  val cellModel = new Model(height, width)

  val table = new Table(height, width) {
    rowHeight = 25
    autoResizeMode = Table.AutoResizeMode.Off
    showGrid = true
    gridColor = new java.awt.Color(150, 150, 150)

    override def rendererComponent(isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
      if (hasFocus)
        new TextField(userData(row, column))
      else
        new Label(cellModel.cells(row)(column).toString) {
          xAlignment = Alignment.Right
        }

    def userData(row: Int, column: Int): String = {
      val v = this (row, column)
      if (v == null) "" else v.toString
    }

    reactions += {
      case TableUpdated(_table, rows, column) =>
        for (row <- rows)
          cellModel.cells(row)(column).formula =
            FormulaParsers.parse(userData(row, column))
    }
  }

  val rowHeader =
    new ListView((0 until height) map (_.toString)) {
      fixedCellWidth = 30
      fixedCellHeight = table.rowHeight
    }

  viewportView = table
  rowHeaderView = rowHeader
}